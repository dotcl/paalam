;;;; main.lisp — paalam 画像ビューア本体 (Avalonia バックエンド; 定義のみ)
;;;;
;;;; GUI は Avalonia (OSS, MIT, クロスプラット)。dotcl host (paalam.exe) が
;;;; DotclHost.Call("RUN-PAALAM" target) を呼び、Lisp 側で AppBuilder を起動する。
;;;; net10.0 (-windows 不要)・STA 不要・Windows Desktop ランタイム非依存。
;;;;
;;;; 「本 (book)」= ページの供給源 (ディレクトリ / アーカイブ) を抽象化した
;;;; CLOS プロトコル。directory-book / zip-book が実装する。GUI 非依存。

(defpackage :paalam
  (:use :cl)
  (:export :run-paalam :build-main-window :open-book
           :book-page-count :book-page-name :book-load-image :book-close))
(in-package :paalam)

;;; define-class は DotCL.Runtime NuGet に同梱されないので vendor して load。
;;; *package* = :paalam で読むことで、マクロが束縛する `self' が
;;; この下の define-class 本体 (:paalam) と同一シンボルになる。
(load (concatenate 'string
                   (dotnet:static "System.AppContext" "BaseDirectory")
                   "vendor-dotnet-class.lisp"))

(defun log-err (where e)
  (ignore-errors
    (with-open-file (s (concatenate 'string
                                    (dotnet:static "System.AppContext" "BaseDirectory")
                                    "paalam-error.log")
                       :direction :output :if-exists :append :if-does-not-exist :create)
      (format s "[~a] ~a~%" where e))))

;;; ------------------------------------------------------------------
;;; ファイル名ユーティリティ
;;; ------------------------------------------------------------------

(defparameter *image-extensions* '("jpg" "jpeg" "png" "gif" "bmp" "tif" "tiff" "webp")
  "対応画像拡張子 (小文字)。Avalonia/Skia は webp も読める。")

(defun image-ext-p (ext)
  (and ext (member (string-downcase ext) *image-extensions* :test #'string=) t))

(defun image-name-p (name)
  (let ((dot (position #\. name :from-end t)))
    (and dot (image-ext-p (subseq name (1+ dot))))))

(defun natural-key (s)
  "自然順ソート用キー。数字列をゼロ詰めし英字は小文字化 (page2 < page10)。"
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
;;; 画像ロード (Avalonia.Media.Imaging.Bitmap)
;;; ------------------------------------------------------------------

(defun %load-file-bitmap (path)
  (dotnet:new "Avalonia.Media.Imaging.Bitmap" path))   ; Bitmap(string fileName)

(defun %load-zip-bitmap (entry)
  ;; ZipArchiveEntry.Open() は非 seekable なので MemoryStream に写してから渡す。
  ;; Avalonia Bitmap(Stream) は ctor 内で復号するので、戻り後に stream を閉じてよい。
  (let ((es (dotnet:invoke entry "Open"))
        (ms (dotnet:new "System.IO.MemoryStream")))
    (unwind-protect
         (progn (dotnet:invoke es "CopyTo" ms)
                (dotnet:invoke ms "set_Position" 0)
                ;; dotnet:new は Bitmap(string)/(Stream) を区別できないので
                ;; C# シム経由で Stream ctor を確実に呼ぶ。
                (dotnet:static "Paalam.Imaging" "BitmapFromStream" ms))
      (dotnet:invoke es "Dispose")
      (dotnet:invoke ms "Dispose"))))

;;; ------------------------------------------------------------------
;;; book プロトコル
;;; ------------------------------------------------------------------

(defgeneric book-page-count (book)        (:documentation "ページ数。"))
(defgeneric book-page-name  (book index)  (:documentation "index ページの表示名。"))
(defgeneric book-load-image (book index)  (:documentation "index ページの Avalonia Bitmap。"))
(defgeneric book-close      (book)        (:documentation "リソース解放。"))

;;; directory-book ---------------------------------------------------

(defclass directory-book ()
  ((paths :initarg :paths :reader db-paths)))

(defun %ensure-trailing-slash (dir)
  (if (and (plusp (length dir))
           (member (char dir (1- (length dir))) '(#\/ #\\)))
      dir
      (concatenate 'string dir "/")))

(defun %dir-image-files (dir)
  (let ((dir (%ensure-trailing-slash dir)))
    (sort (loop for ext in *image-extensions*
                append (mapcar #'namestring
                               (directory (concatenate 'string dir "*." ext))))
          #'natural<)))

(defun make-directory-book (dir)
  (make-instance 'directory-book :paths (coerce (%dir-image-files dir) 'vector)))

(defmethod book-page-count ((b directory-book)) (length (db-paths b)))
(defmethod book-page-name  ((b directory-book) i) (file-namestring (aref (db-paths b) i)))
(defmethod book-load-image ((b directory-book) i) (%load-file-bitmap (aref (db-paths b) i)))
(defmethod book-close      ((b directory-book)) nil)

;;; zip-book ---------------------------------------------------------

(defclass zip-book ()
  ((stream  :initarg :stream  :reader zb-stream)
   (archive :initarg :archive :reader zb-archive)
   (names   :initarg :names   :reader zb-names)
   (entries :initarg :entries :reader zb-entries)))

(defun %zip-image-entries (zip)
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
2 値 (book start-index) を返す。"
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
;;; viewer: book + 現在ページ + UI 結線
;;; ------------------------------------------------------------------

(defstruct viewer book (index 0) image window)

(defparameter *page-direction* :rtl
  "綴じ方向。:rtl = NeeView 既定 (右開き/漫画) で ← が次・→ が前。:ltr は逆。")

(defun show-page (vw i)
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
  "Window KeyDown ハンドラ。←/→/PageUp/PageDown/Space/Home/End でページ送り。
左右矢印は *page-direction* で反転。"
  (let ((kn (dotnet:invoke (dotnet:invoke e "get_Key") "ToString")))
    (flet ((next () (show-page vw (1+ (viewer-index vw))))
           (prev () (show-page vw (1- (viewer-index vw)))))
      (cond
        ((member kn '("PageDown" "Next" "Space") :test #'string=) (next))
        ((member kn '("PageUp" "Prior" "Back") :test #'string=) (prev))
        ((string= kn "Left")  (if (eq *page-direction* :rtl) (next) (prev)))
        ((string= kn "Right") (if (eq *page-direction* :rtl) (prev) (next)))
        ((string= kn "Home") (show-page vw 0))
        ((string= kn "End")  (show-page vw (1- (book-page-count (viewer-book vw)))))))))

;;; ------------------------------------------------------------------
;;; ウィンドウ / アプリ (Avalonia)
;;; ------------------------------------------------------------------

(defparameter *default-image* "C:/Windows/System32/@WLOGO_96x96.png"
  "対象未指定時のフォールバック (= その親フォルダを本として開く)。")

(defvar *target-path* nil
  "RUN-PAALAM が受け取った対象パス (App は Avalonia が生成するので global で渡す)。")

(defun %resolve-path (path)
  (or (and (stringp path) (plusp (length path)) path)
      (let ((env (dotnet:static "System.Environment" "GetEnvironmentVariable" "PAALAM_PATH")))
        (and env (plusp (length env)) env))
      *default-image*))

(defun build-main-window ()
  "現在の *target-path* の本を開き、Avalonia Window を返す (呼び出しスレッド上で構築)。"
  (multiple-value-bind (book start) (open-book (%resolve-path *target-path*))
    (let* ((win (dotnet:new "Avalonia.Controls.Window"))
           (img (dotnet:new "Avalonia.Controls.Image"))
           (vw  (make-viewer :book book :index start :image img :window win)))
      (dotnet:invoke img "set_Stretch" (dotnet:static "Avalonia.Media.Stretch" "Uniform"))
      (dotnet:invoke win "set_Content" img)
      (dotnet:invoke win "set_Width"  1000d0)
      (dotnet:invoke win "set_Height" 720d0)
      (dotnet:add-event win "KeyDown"
                        (lambda (s e) (declare (ignore s)) (%on-key vw e)))
      (dotnet:add-event win "Closed"
                        (lambda (s e) (declare (ignore s e)) (book-close book)))
      (show-page vw start)
      win)))

(defun %maybe-autoclose (life)
  "DOTCL_AUTOCLOSE_MS があれば その ms 後に lifetime.Shutdown (動作確認用)。"
  (let ((s (dotnet:static "System.Environment" "GetEnvironmentVariable" "DOTCL_AUTOCLOSE_MS")))
    (when (and s (plusp (length s)))
      (let ((ms (parse-integer s :junk-allowed t))
            (timer (dotnet:new "Avalonia.Threading.DispatcherTimer")))
        (when ms
          (dotnet:invoke timer "set_Interval"
                         (dotnet:static "System.TimeSpan" "FromMilliseconds"
                                        (coerce ms 'double-float)))
          (dotnet:add-event timer "Tick"
                            (lambda (sndr e) (declare (ignore sndr e))
                              (dotnet:invoke timer "Stop")
                              (dotnet:invoke life "Shutdown" 0)))
          (dotnet:invoke timer "Start"))))))

;;; Avalonia.Application 派生を Lisp 側で emit。
(dotnet:define-class "Paalam.App" ("Avalonia.Application")
  (:ctor ()
    ;; Fluent テーマ (将来のメニュー/ツールバー等テンプレートコントロール用)。
    ;; Avalonia 12 の FluentTheme は引数なし ctor が無いので省略可引数 nil を渡す (#292)。
    (handler-case
        (let ((styles (dotnet:invoke self "get_Styles"))
              (theme (dotnet:new "Avalonia.Themes.Fluent.FluentTheme" nil)))
          (dotnet:invoke styles "Add" theme))
      (error (e) (log-err "ctor" e))))
  (:methods
    ("OnFrameworkInitializationCompleted" () :returns Void :override t
      (handler-case
          (let ((life (dotnet:invoke self "get_ApplicationLifetime"))
                (win  (build-main-window)))
            (dotnet:invoke life "set_MainWindow" win)
            (%maybe-autoclose life))
        (error (e) (log-err "init" e))))))

(defun run-paalam (&optional target)
  "host 入口。AppBuilder.Configure<Paalam.App>().UsePlatformDetect()
   .StartWithClassicDesktopLifetime(args) を駆動する (窓が閉じるまで blocking)。"
  (setf *target-path* target)
  (let* ((b    (dotnet:static-generic "Avalonia.AppBuilder" "Configure" (list "Paalam.App")))
         (b    (dotnet:static "Avalonia.AppBuilderDesktopExtensions" "UsePlatformDetect" b))
         (args (dotnet:static-generic "System.Array" "Empty" (list "System.String"))))
    (dotnet:static "Avalonia.ClassicDesktopStyleApplicationLifetimeExtensions"
                   "StartWithClassicDesktopLifetime"
                   b args
                   (dotnet:static "Avalonia.Controls.ShutdownMode" "OnLastWindowClose"))))
