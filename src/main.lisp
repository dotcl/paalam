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

;;; --- 表示設定 (将来のカスタマイズ UI / 設定ファイルの対象) ---------
(defparameter *page-direction* :rtl
  "綴じ方向。:rtl = NeeView 既定 (右開き/漫画) で ← が次・→ が前。:ltr は逆。")
(defparameter *spread* t
  "見開き表示。t = 2 ページ並べ、nil = 単ページ。")
(defparameter *first-page-single* t
  "見開き時、先頭ページ (表紙) を単独表示する (漫画の慣習)。")

;;; --- 設定の永続化 (s式 plist / OS の app data) ---------------------
(defparameter *settings*
  '((:page-direction    . *page-direction*)
    (:spread            . *spread*)
    (:first-page-single . *first-page-single*))
  "保存対象の (設定キー . 特殊変数シンボル)。ここに足すだけで永続化対象になる。")

(defun %config-dir ()
  (concatenate 'string
               (dotnet:static "System.Environment" "GetFolderPath"
                              (dotnet:static "System.Environment+SpecialFolder" "ApplicationData"))
               "/paalam/"))

(defun %config-file () (concatenate 'string (%config-dir) "settings.lisp"))

(defun save-settings ()
  "現在の設定値を s式 plist で書き出す。"
  (handler-case
      (progn
        (dotnet:static "System.IO.Directory" "CreateDirectory" (%config-dir))
        (with-open-file (s (%config-file) :direction :output
                           :if-exists :supersede :if-does-not-exist :create)
          (let ((plist '()))
            (dolist (e *settings*)
              (setf plist (list* (car e) (symbol-value (cdr e)) plist)))
            (with-standard-io-syntax (prin1 plist s) (terpri s)))))
    (error (e) (log-err "save-settings" e))))

(defun load-settings ()
  "設定ファイルがあれば読み込んで反映。無ければ/壊れていれば既定値で作り直す。"
  (let ((file (%config-file)))
    (handler-case
        (if (dotnet:static "System.IO.File" "Exists" file)
            (with-open-file (s file :direction :input)
              (let* ((*read-eval* nil) (plist (read s nil nil)))
                (when (consp plist)
                  (loop for (k v) on plist by #'cddr
                        for e = (assoc k *settings*)
                        when e do (setf (symbol-value (cdr e)) v)))))
            (save-settings))
      (error (e)
        (log-err "load-settings" e)
        (ignore-errors (save-settings))))))   ; 壊れていたら既定で作り直す

;;; viewer: book + 見開き単位 (spreads) + 現在位置 + 2 枚の Image
;;; spreads = 各見開きの page-index リスト (1 or 2 個) の vector。
;;; 単ページ/見開きを同じ仕組みで扱う (単ページ時は全要素 1 個)。
;;; zoom/tx/ty + scale-xf/trans-xf は stage の RenderTransform (ズーム/パン)。
(defstruct viewer book spreads (sindex 0) window left-img right-img
           viewport scale-xf trans-xf (zoom 1d0) (tx 0d0) (ty 0d0)
           (dragging nil) (lx 0d0) (ly 0d0))

(defun apply-transform (vw)
  (dotnet:invoke (viewer-scale-xf vw) "set_ScaleX" (viewer-zoom vw))
  (dotnet:invoke (viewer-scale-xf vw) "set_ScaleY" (viewer-zoom vw))
  (dotnet:invoke (viewer-trans-xf vw) "set_X" (viewer-tx vw))
  (dotnet:invoke (viewer-trans-xf vw) "set_Y" (viewer-ty vw)))

(defun reset-view (vw)
  "ズーム/パンをフィット (等倍・原点) に戻す。"
  (setf (viewer-zoom vw) 1d0 (viewer-tx vw) 0d0 (viewer-ty vw) 0d0)
  (apply-transform vw))

(defun compute-spreads (n)
  "n ページを *spread* / *first-page-single* に従って見開き単位へ分割。"
  (let ((spreads '()) (i 0))
    (cond
      ((<= n 0))
      ((not *spread*)
       (loop for k from 0 below n do (push (list k) spreads)))
      (t
       (when *first-page-single*
         (push (list 0) spreads) (setf i 1))
       (loop while (< i n) do
         (if (< (1+ i) n)
             (progn (push (list i (1+ i)) spreads) (incf i 2))
             (progn (push (list i) spreads) (incf i))))))
    (coerce (nreverse spreads) 'vector)))

(defun spread-index-of (spreads page)
  "page を含む見開きの index (見つからなければ 0)。"
  (or (position-if (lambda (pages) (member page pages)) spreads) 0))

(defun %halign (name) (dotnet:static "Avalonia.Layout.HorizontalAlignment" name))

(defun %place-image (img source col span halign)
  (dotnet:invoke img "set_Source" source)
  (dotnet:invoke img "set_IsVisible" (and source t))
  (dotnet:static "Avalonia.Controls.Grid" "SetColumn" img col)
  (dotnet:static "Avalonia.Controls.Grid" "SetColumnSpan" img span)
  (dotnet:invoke img "set_HorizontalAlignment" (%halign halign)))

(defun %spread-title (vw pages)
  (let* ((book (viewer-book vw)) (n (book-page-count book)))
    (if (= (length pages) 1)
        (format nil "paalam — ~a (~d/~d)"
                (book-page-name book (first pages)) (1+ (first pages)) n)
        (destructuring-bind (lo hi) pages
          (format nil "paalam — ~a｜~a (~d-~d/~d)"
                  (book-page-name book lo) (book-page-name book hi) (1+ lo) (1+ hi) n)))))

(defun show-spread (vw)
  "現在の見開きを描画 (sindex は範囲クランプ)。"
  (let* ((spreads (viewer-spreads vw)) (m (length spreads)))
    (when (plusp m)
      (let* ((si (max 0 (min (viewer-sindex vw) (1- m))))
             (pages (aref spreads si))
             (book (viewer-book vw))
             (li (viewer-left-img vw))
             (ri (viewer-right-img vw)))
        (setf (viewer-sindex vw) si)
        (if (= (length pages) 1)
            (progn
              (%place-image li (book-load-image book (first pages)) 0 2 "Center")
              (%place-image ri nil 1 1 "Left"))
            (destructuring-bind (lo hi) pages
              ;; rtl(右開き): 下位 index=lo を右に、hi を左に。ltr は逆。
              (let ((left-page  (if (eq *page-direction* :rtl) hi lo))
                    (right-page (if (eq *page-direction* :rtl) lo hi)))
                (%place-image li (book-load-image book left-page)  0 1 "Right")
                (%place-image ri (book-load-image book right-page) 1 1 "Left"))))
        (dotnet:invoke (viewer-window vw) "set_Title" (%spread-title vw pages))
        (reset-view vw)))))

(defun %recompute (vw keep-page)
  "spreads を計算し直し、keep-page を含む見開きへ位置を合わせて再描画。"
  (setf (viewer-spreads vw) (compute-spreads (book-page-count (viewer-book vw))))
  (setf (viewer-sindex vw) (spread-index-of (viewer-spreads vw) keep-page))
  (show-spread vw))

(defun %cur-page (vw)
  (let ((sp (viewer-spreads vw)))
    (if (plusp (length sp)) (first (aref sp (viewer-sindex vw))) 0)))

(defparameter *zoom-step* 1.2d0 "ホイール 1 ノッチあたりの倍率。")
(defparameter *zoom-min* 0.1d0)
(defparameter *zoom-max* 10d0)

(defun %pos-xy (e elem)
  "PointerEventArgs の elem 基準座標を (values x y) で。"
  (let ((p (dotnet:invoke e "GetPosition" elem)))
    (values (dotnet:invoke p "get_X") (dotnet:invoke p "get_Y"))))

(defun %on-wheel (vw e)
  "ホイールズーム。カーソル位置を固定点にして拡大縮小する。"
  (let* ((dy (dotnet:invoke (dotnet:invoke e "get_Delta") "get_Y"))
         (s  (viewer-zoom vw))
         (s2 (max *zoom-min* (min *zoom-max* (* s (if (> dy 0) *zoom-step* (/ 1d0 *zoom-step*)))))))
    (multiple-value-bind (cx cy) (%pos-xy e (viewer-viewport vw))
      ;; t' = c - (s'/s)(c - t)  (origin top-left, screen = s*p + t)
      (let ((r (/ s2 s)))
        (setf (viewer-tx vw) (- cx (* r (- cx (viewer-tx vw))))
              (viewer-ty vw) (- cy (* r (- cy (viewer-ty vw))))
              (viewer-zoom vw) s2)))
    (apply-transform vw)))

(defun %on-press (vw e)
  (multiple-value-bind (x y) (%pos-xy e (viewer-viewport vw))
    (setf (viewer-dragging vw) t (viewer-lx vw) x (viewer-ly vw) y)))

(defun %on-move (vw e)
  (when (viewer-dragging vw)
    (multiple-value-bind (x y) (%pos-xy e (viewer-viewport vw))
      (incf (viewer-tx vw) (- x (viewer-lx vw)))
      (incf (viewer-ty vw) (- y (viewer-ly vw)))
      (setf (viewer-lx vw) x (viewer-ly vw) y)
      (apply-transform vw))))

(defun %on-release (vw e) (declare (ignore e)) (setf (viewer-dragging vw) nil))

(defun %on-key (vw e)
  "Window KeyDown。←/→/PageUp/Down/Space/Home/End で送り、S=見開き切替, D=綴じ方向切替, 0=フィット。"
  (let ((kn (dotnet:invoke (dotnet:invoke e "get_Key") "ToString")))
    (flet ((nexts () (incf (viewer-sindex vw)) (show-spread vw))
           (prevs () (decf (viewer-sindex vw)) (show-spread vw)))
      (cond
        ((member kn '("PageDown" "Next" "Space") :test #'string=) (nexts))
        ((member kn '("PageUp" "Prior" "Back") :test #'string=) (prevs))
        ((string= kn "Left")  (if (eq *page-direction* :rtl) (nexts) (prevs)))
        ((string= kn "Right") (if (eq *page-direction* :rtl) (prevs) (nexts)))
        ((string= kn "Home") (setf (viewer-sindex vw) 0) (show-spread vw))
        ((string= kn "End")  (setf (viewer-sindex vw) (1- (length (viewer-spreads vw))))
                             (show-spread vw))
        ((string= kn "S") (setf *spread* (not *spread*)) (%recompute vw (%cur-page vw))
                          (save-settings))
        ((string= kn "D") (setf *page-direction* (if (eq *page-direction* :rtl) :ltr :rtl))
                          (show-spread vw) (save-settings))
        ((member kn '("D0" "NumPad0") :test #'string=) (reset-view vw))))))

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
  "現在の *target-path* の本を開き、見開き対応の Avalonia Window を返す
(呼び出しスレッド上で構築)。2 列 Grid に 2 枚の Image を中央で突き合わせる。"
  (multiple-value-bind (book start) (open-book (%resolve-path *target-path*))
    (let* ((win   (dotnet:new "Avalonia.Controls.Window"))
           (vport (dotnet:new "Avalonia.Controls.Border"))   ; クリップ用ビューポート
           (grid  (dotnet:new "Avalonia.Controls.Grid"))     ; 見開き stage
           (li    (dotnet:new "Avalonia.Controls.Image"))
           (ri    (dotnet:new "Avalonia.Controls.Image"))
           (scale (dotnet:new "Avalonia.Media.ScaleTransform"))
           (trans (dotnet:new "Avalonia.Media.TranslateTransform"))
           (grp   (dotnet:new "Avalonia.Media.TransformGroup"))
           (spreads (compute-spreads (book-page-count book)))
           (vw   (make-viewer :book book :spreads spreads
                              :sindex (spread-index-of spreads start)
                              :window win :left-img li :right-img ri
                              :viewport vport :scale-xf scale :trans-xf trans)))
      ;; 2 つの star 列 (既定 ColumnDefinition = 1*)
      (let ((cols (dotnet:invoke grid "get_ColumnDefinitions")))
        (dotnet:invoke cols "Add" (dotnet:new "Avalonia.Controls.ColumnDefinition"))
        (dotnet:invoke cols "Add" (dotnet:new "Avalonia.Controls.ColumnDefinition")))
      (dolist (im (list li ri))
        (dotnet:invoke im "set_Stretch" (dotnet:static "Avalonia.Media.Stretch" "Uniform"))
        (dotnet:invoke im "set_VerticalAlignment"
                       (dotnet:static "Avalonia.Layout.VerticalAlignment" "Center")))
      (let ((children (dotnet:invoke grid "get_Children")))
        (dotnet:invoke children "Add" li)
        (dotnet:invoke children "Add" ri))
      ;; stage に RenderTransform (ズーム/パン)。原点は左上で translate を親座標系に。
      (let ((gc (dotnet:invoke grp "get_Children")))
        (dotnet:invoke gc "Add" scale)
        (dotnet:invoke gc "Add" trans))
      (dotnet:invoke grid "set_RenderTransform" grp)
      (dotnet:invoke grid "set_RenderTransformOrigin"
                     (dotnet:static "Avalonia.RelativePoint" "TopLeft"))
      ;; viewport (clip) > stage
      (dotnet:invoke vport "set_ClipToBounds" t)
      (dotnet:invoke vport "set_Child" grid)
      (dotnet:invoke win "set_Content" vport)
      (dotnet:invoke win "set_Background" (dotnet:static "Avalonia.Media.Brushes" "Black"))
      (dotnet:invoke win "set_Width"  1200d0)
      (dotnet:invoke win "set_Height" 800d0)
      (dotnet:add-event win "KeyDown"
                        (lambda (s e) (declare (ignore s)) (%on-key vw e)))
      (dotnet:add-event win "PointerWheelChanged"
                        (lambda (s e) (declare (ignore s)) (%on-wheel vw e)))
      (dotnet:add-event win "PointerPressed"
                        (lambda (s e) (declare (ignore s)) (%on-press vw e)))
      (dotnet:add-event win "PointerMoved"
                        (lambda (s e) (declare (ignore s)) (%on-move vw e)))
      (dotnet:add-event win "PointerReleased"
                        (lambda (s e) (declare (ignore s)) (%on-release vw e)))
      (dotnet:add-event win "Closed"
                        (lambda (s e) (declare (ignore s e)) (book-close book)))
      (show-spread vw)
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
  (load-settings)
  (setf *target-path* target)
  (let* ((b    (dotnet:static-generic "Avalonia.AppBuilder" "Configure" (list "Paalam.App")))
         (b    (dotnet:static "Avalonia.AppBuilderDesktopExtensions" "UsePlatformDetect" b))
         (args (dotnet:static-generic "System.Array" "Empty" (list "System.String"))))
    (dotnet:static "Avalonia.ClassicDesktopStyleApplicationLifetimeExtensions"
                   "StartWithClassicDesktopLifetime"
                   b args
                   (dotnet:static "Avalonia.Controls.ShutdownMode" "OnLastWindowClose"))))
