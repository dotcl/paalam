;;;; main.lisp — paalam 画像ビューア本体 (定義のみ; 副作用なし)
;;;;
;;;; 起動経路:
;;;;  - 配布: paalam.exe (WPF host) が DotclHost.Call("BUILD-APP" target) を呼ぶ。
;;;;    C# 側 [STAThread] Main 上で Application を作って Run する。
;;;;  - 開発: dotcl から experiments/dev-run.lisp が (build-main-window) を
;;;;    自前 STA スレッドで Show する (高速反復用)。
;;;;
;;;; 「本 (book)」= ページの供給源 (ディレクトリ / アーカイブ) を抽象化した
;;;; CLOS プロトコル。directory-book / zip-book が実装する。ページ送りは
;;;; book-* だけ触るので backend が増えても無改修。

(defpackage :paalam
  (:use :cl)
  (:export :build-app :build-main-window :open-book :ensure-wpf
           :book-page-count :book-page-name :book-load-image :book-close))
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; WPF 可用性 (host=単純名で解決 / script=フルパス fallback)
;;; ------------------------------------------------------------------

(defvar *wpf-ready* nil)

(defun %latest-windowsdesktop-dir ()
  (let ((dirs (directory
               "C:/Program Files/dotnet/shared/Microsoft.WindowsDesktop.App/*/")))
    (when dirs
      (flet ((ver (p) (car (last (pathname-directory p)))))
        (let ((best (first dirs)))
          (dolist (p (rest dirs)) (when (string< (ver best) (ver p)) (setf best p)))
          (namestring best))))))

(defun ensure-wpf ()
  "WPF アセンブリを利用可能にする。host では型が単純名で解決でき、script では
WindowsDesktop.App からフルパス load する。一度成功したら no-op。"
  (when *wpf-ready* (return-from ensure-wpf t))
  ;; 型が解決できるか静的プロパティで試す (Application を new しない —
  ;; WPF は 1 AppDomain 1 Application なので probe で消費してはいけない)。
  (handler-case
      (progn
        (dotnet:static "System.Windows.Application" "Current")
        (setf *wpf-ready* t))
    (error ()
      (let ((dir (%latest-windowsdesktop-dir)))
        (unless dir (error "Microsoft.WindowsDesktop.App not found"))
        (dolist (dll '("WindowsBase.dll" "PresentationCore.dll"
                       "PresentationFramework.dll" "System.Xaml.dll"))
          (dotnet:load-assembly (concatenate 'string dir dll)))
        (setf *wpf-ready* t))))
  *wpf-ready*)

;;; ------------------------------------------------------------------
;;; ファイル名ユーティリティ
;;; ------------------------------------------------------------------

(defparameter *image-extensions* '("jpg" "jpeg" "png" "gif" "bmp" "tif" "tiff")
  "対応画像拡張子 (小文字)。後で webp/avif を SkiaSharp 等で追加予定。")

(defun image-ext-p (ext)
  (and ext (member (string-downcase ext) *image-extensions* :test #'string=) t))

(defun image-name-p (name)
  "ファイル名 (or zip エントリ名) が画像拡張子で終わるか。"
  (let ((dot (position #\. name :from-end t)))
    (and dot (image-ext-p (subseq name (1+ dot))))))

(defun natural-key (s)
  "自然順ソート用キー。数字列をゼロ詰めし、英字は小文字化する。
これで page2 < page10 になる。"
  (with-output-to-string (out)
    (let ((i 0) (n (length s)))
      (loop while (< i n) do
        (let ((c (char s i)))
          (if (digit-char-p c)
              (let ((j i))
                (loop while (and (< j n) (digit-char-p (char s j))) do (incf j))
                (format out "~20,'0d" (parse-integer (subseq s i j)))
                (setf i j))
              (progn (write-char (char-downcase c) out) (incf i))))))))

(defun natural< (a b) (string< (natural-key a) (natural-key b)))

;;; ------------------------------------------------------------------
;;; 画像ロード (WPF BitmapImage)。stream 経由 + OnLoad でファイルロックを残さない。
;;; ------------------------------------------------------------------

(defun %bitmap-from-stream (stream)
  "seekable な stream から BitmapImage を全読み込み (OnLoad) で作る。
EndInit で復号完了するので、戻り後に stream を閉じてよい。"
  (let ((bmp (dotnet:new "System.Windows.Media.Imaging.BitmapImage")))
    (dotnet:invoke bmp "BeginInit")
    (dotnet:invoke bmp "set_CacheOption"
                   (dotnet:static "System.Windows.Media.Imaging.BitmapCacheOption" "OnLoad"))
    (dotnet:invoke bmp "set_StreamSource" stream)
    (dotnet:invoke bmp "EndInit")
    (dotnet:invoke bmp "Freeze")
    bmp))

(defun %load-file-bitmap (path)
  (let ((fs (dotnet:static "System.IO.File" "OpenRead" path)))
    (unwind-protect (%bitmap-from-stream fs)
      (dotnet:invoke fs "Dispose"))))

(defun %load-zip-bitmap (entry)
  ;; ZipArchiveEntry.Open() は非 seekable なので MemoryStream に写してから渡す。
  (let ((es (dotnet:invoke entry "Open"))
        (ms (dotnet:new "System.IO.MemoryStream")))
    (unwind-protect
         (progn (dotnet:invoke es "CopyTo" ms)
                (dotnet:invoke ms "set_Position" 0)
                (%bitmap-from-stream ms))
      (dotnet:invoke es "Dispose")
      (dotnet:invoke ms "Dispose"))))

;;; ------------------------------------------------------------------
;;; book プロトコル
;;; ------------------------------------------------------------------

(defgeneric book-page-count (book)        (:documentation "ページ数。"))
(defgeneric book-page-name  (book index)  (:documentation "index ページの表示名。"))
(defgeneric book-load-image (book index)  (:documentation "index ページの WPF BitmapImage。"))
(defgeneric book-close      (book)        (:documentation "リソース解放。"))

;;; directory-book ---------------------------------------------------

(defclass directory-book ()
  ((paths :initarg :paths :reader db-paths)))  ; 画像ファイル full path の vector

(defun %ensure-trailing-slash (dir)
  (if (and (plusp (length dir))
           (member (char dir (1- (length dir))) '(#\/ #\\)))
      dir
      (concatenate 'string dir "/")))

(defun %dir-image-files (dir)
  "dir 直下の画像ファイル full path を自然順ソートして返す。"
  (let ((dir (%ensure-trailing-slash dir)))
    (sort (loop for ext in *image-extensions*
                append (mapcar #'namestring
                               (directory (concatenate 'string dir "*." ext))))
          #'natural<)))

(defun make-directory-book (dir)
  (make-instance 'directory-book
                 :paths (coerce (%dir-image-files dir) 'vector)))

(defmethod book-page-count ((b directory-book)) (length (db-paths b)))
(defmethod book-page-name  ((b directory-book) i) (file-namestring (aref (db-paths b) i)))
(defmethod book-load-image ((b directory-book) i) (%load-file-bitmap (aref (db-paths b) i)))
(defmethod book-close      ((b directory-book)) nil)

;;; zip-book ---------------------------------------------------------

(defclass zip-book ()
  ((stream  :initarg :stream  :reader zb-stream)
   (archive :initarg :archive :reader zb-archive)
   (names   :initarg :names   :reader zb-names)     ; FullName の vector
   (entries :initarg :entries :reader zb-entries))) ; ZipArchiveEntry の vector

(defun %zip-image-entries (zip)
  "zip 内の画像エントリを (name . entry) の自然順ソート済みリストで返す。"
  (let* ((entries (dotnet:invoke zip "get_Entries"))
         (n (dotnet:invoke entries "get_Count"))
         (acc '()))
    (dotimes (i n)
      (let* ((e (dotnet:invoke entries "get_Item" i))
             (name (dotnet:invoke e "get_FullName")))
        (when (image-name-p name) (push (cons name e) acc))))
    (sort (nreverse acc) #'natural< :key #'car)))

(defun make-zip-book (path)
  (ignore-errors (dotnet:load-assembly "System.IO.Compression"))
  (let* ((fs  (dotnet:static "System.IO.File" "OpenRead" path))
         (zip (dotnet:new "System.IO.Compression.ZipArchive" fs))
         (pairs (%zip-image-entries zip)))
    (make-instance 'zip-book
                   :stream fs :archive zip
                   :names   (coerce (mapcar #'car pairs) 'vector)
                   :entries (coerce (mapcar #'cdr pairs) 'vector))))

(defmethod book-page-count ((b zip-book)) (length (zb-entries b)))
(defmethod book-page-name  ((b zip-book) i) (file-namestring (aref (zb-names b) i)))
(defmethod book-load-image ((b zip-book) i) (%load-zip-bitmap (aref (zb-entries b) i)))
(defmethod book-close ((b zip-book))
  (ignore-errors (dotnet:invoke (zb-archive b) "Dispose"))
  (ignore-errors (dotnet:invoke (zb-stream b) "Dispose")))

;;; open-book --------------------------------------------------------

(defun open-book (path)
  "PATH (ディレクトリ / アーカイブ / 画像ファイル) を本として開く。
2 値 (book start-index) を返す。画像ファイル単体は親フォルダを本にして
そのファイルを初期ページにする (NeeView と同じ挙動)。"
  (cond
    ((dotnet:static "System.IO.Directory" "Exists" path)
     (values (make-directory-book path) 0))
    ((dotnet:static "System.IO.File" "Exists" path)
     (let ((ext (pathname-type path)))
       (cond
         ((and ext (member (string-downcase ext) '("zip" "cbz") :test #'string=))
          (values (make-zip-book path) 0))
         ((image-ext-p ext)
          (let* ((dir  (directory-namestring path))
                 (book (make-directory-book dir))
                 (start (or (position (file-namestring path) (db-paths book)
                                      :key #'file-namestring :test #'string-equal)
                            0)))
            (values book start)))
         (t (error "対応していないファイル形式: ~a" path)))))
    (t (error "パスが見つかりません: ~a" path))))

;;; ------------------------------------------------------------------
;;; viewer: book + 現在ページ + UI への結線
;;; ------------------------------------------------------------------

(defstruct viewer book (index 0) image window)

(defun show-page (vw i)
  "ページ i を表示 (範囲はクランプ)。"
  (let* ((book (viewer-book vw))
         (n (book-page-count book)))
    (when (plusp n)
      (let ((i (max 0 (min i (1- n)))))
        (setf (viewer-index vw) i)
        (dotnet:invoke (viewer-image vw) "set_Source" (book-load-image book i))
        (dotnet:invoke (viewer-window vw) "set_Title"
                       (format nil "paalam — ~a (~d/~d)"
                               (book-page-name book i) (1+ i) n))))))

(defun %on-key (vw e)
  "Window KeyDown ハンドラ。←/→/PageUp/PageDown/Space/Home/End でページ送り。"
  (let ((kn (dotnet:invoke (dotnet:invoke e "get_Key") "ToString")))
    (cond
      ((member kn '("Right" "PageDown" "Next" "Space") :test #'string=)
       (show-page vw (1+ (viewer-index vw))))
      ((member kn '("Left" "PageUp" "Prior" "Back") :test #'string=)
       (show-page vw (1- (viewer-index vw))))
      ((string= kn "Home") (show-page vw 0))
      ((string= kn "End")  (show-page vw (1- (book-page-count (viewer-book vw))))))))

;;; ------------------------------------------------------------------
;;; ウィンドウ / アプリ構築
;;; ------------------------------------------------------------------

(defparameter *default-image* "C:/Windows/System32/@WLOGO_96x96.png"
  "対象未指定時に開くフォールバック (= System32 フォルダを本として開く)。")

(defun %resolve-path (path)
  "host から渡された path (nil 可) → env PAALAM_PATH → デフォルト の順で確定。"
  (or (and (stringp path) (plusp (length path)) path)
      (let ((env (dotnet:static "System.Environment" "GetEnvironmentVariable" "PAALAM_PATH")))
        (and env (plusp (length env)) env))
      *default-image*))

(defun build-main-window (&optional path)
  "PATH の本を開き、画像 1 枚を Uniform フィットで表示する Window を返す。
呼び出しスレッド (STA) 上で構築する。"
  (ensure-wpf)
  (multiple-value-bind (book start) (open-book (%resolve-path path))
    (let* ((win  (dotnet:new "System.Windows.Window"))
           (ictl (dotnet:new "System.Windows.Controls.Image"))
           (vw   (make-viewer :book book :index start :image ictl :window win)))
      (dotnet:invoke ictl "set_Stretch"
                     (dotnet:static "System.Windows.Media.Stretch" "Uniform"))
      (dotnet:invoke win "set_Content" ictl)
      (dotnet:invoke win "set_Width"  1000d0)
      (dotnet:invoke win "set_Height" 720d0)
      (dotnet:add-event win "KeyDown"
                        (lambda (s e) (declare (ignore s)) (%on-key vw e)))
      (dotnet:add-event win "Closed"
                        (lambda (s e) (declare (ignore s e)) (book-close book)))
      (show-page vw start)
      win)))

(defun %maybe-autoclose (app)
  "環境変数 DOTCL_AUTOCLOSE_MS があれば その ms 後に app.Shutdown (動作確認用)。"
  (let ((s (dotnet:static "System.Environment" "GetEnvironmentVariable" "DOTCL_AUTOCLOSE_MS")))
    (when (and s (plusp (length s)))
      (let ((ms (parse-integer s :junk-allowed t))
            (timer (dotnet:new "System.Windows.Threading.DispatcherTimer")))
        (when ms
          (dotnet:invoke timer "set_Interval"
                         (dotnet:static "System.TimeSpan" "FromMilliseconds"
                                        (coerce ms 'double-float)))
          (dotnet:add-event timer "Tick"
                            (lambda (sndr e) (declare (ignore sndr e))
                              (dotnet:invoke timer "Stop")
                              (dotnet:invoke app "Shutdown")))
          (dotnet:invoke timer "Start"))))))

(defun build-app (&optional target)
  "WPF host 入口。Application を作り MainWindow を設定して Application を返す。
TARGET は paalam.exe の第 1 引数 (フォルダ/アーカイブ/画像; nil 可)。"
  (ensure-wpf)
  (let ((app (or (dotnet:static "System.Windows.Application" "Current")
                 (dotnet:new "System.Windows.Application")))
        (win (build-main-window target)))
    (dotnet:invoke app "set_MainWindow" win)
    (dotnet:invoke win "Show")
    (%maybe-autoclose app)
    app))
