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
;; 先読みローダー用のスレッド/ロック/条件変数 (bordeaux-threads 相当の contrib)。
(load (concatenate 'string
                   (dotnet:static "System.AppContext" "BaseDirectory")
                   "vendor-dotcl-thread.lisp"))
(use-package :dotcl-thread)

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

(defun %dedup-strings (list)
  (let ((seen (make-hash-table :test 'equal)) (acc '()))
    (dolist (x list (nreverse acc))
      (unless (gethash x seen) (setf (gethash x seen) t) (push x acc)))))

(defun %dir-image-files (dir)
  "dir 直下 (+ *recursive* ならサブフォルダも) の画像ファイル full path を自然順で。
** は直下を含まないので *.ext と **/*.ext の和集合を取る。"
  (let ((dir (%ensure-trailing-slash dir))
        (globs (if *recursive* '("*." "**/*.") '("*.")))
        (out '()))
    (dolist (ext *image-extensions*)
      (dolist (g globs)
        (dolist (p (directory (concatenate 'string dir g ext)))
          (push (namestring p) out))))
    (sort (%dedup-strings out) #'natural<)))

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

;;; sharp-book (SharpCompress: rar/7z/tar 等) -----------------------
;;; zip 以外の書庫を SharpCompress の IArchive 経由で読む。rar/7z/tar など
;;; フォーマット検出は ArchiveFactory.Open が行うので backend は 1 つで済む。

(defun %enum->list (enumerable)
  "IEnumerable を Lisp リストへ (GetEnumerator/MoveNext/Current)。"
  (let ((en (dotnet:invoke enumerable "GetEnumerator")) (acc '()))
    (loop while (dotnet:invoke en "MoveNext")
          do (push (dotnet:invoke en "get_Current") acc))
    (nreverse acc)))

(defun %basename (key)
  "書庫エントリ名 (/ or \\ 区切り) の最終成分。"
  (let ((p (max (or (position #\/ key :from-end t) -1)
                (or (position #\\ key :from-end t) -1))))
    (if (>= p 0) (subseq key (1+ p)) key)))

(defclass sharp-book ()
  ((archive :initarg :archive :reader sb2-archive)
   (names   :initarg :names   :reader sb2-names)
   (entries :initarg :entries :reader sb2-entries)))

(defun %sharp-image-entries (archive)
  (let ((acc '()))
    (dolist (e (%enum->list (dotnet:invoke archive "get_Entries")))
      (unless (dotnet:invoke e "get_IsDirectory")
        (let ((key (dotnet:invoke e "get_Key")))
          (when (and key (image-name-p key)) (push (cons key e) acc)))))
    (sort (nreverse acc) #'natural< :key #'car)))

(defun make-sharp-book (path)
  ;; Open(string, ReaderOptions=null): 省略可引数を明示 nil で渡す (#292 回避)。
  (let* ((archive (dotnet:static "SharpCompress.Archives.ArchiveFactory" "Open" path nil))
         (pairs (%sharp-image-entries archive)))
    (make-instance 'sharp-book
                   :archive archive
                   :names   (coerce (mapcar #'car pairs) 'vector)
                   :entries (coerce (mapcar #'cdr pairs) 'vector))))

(defmethod book-page-count ((b sharp-book)) (length (sb2-entries b)))
(defmethod book-page-name  ((b sharp-book) i) (%basename (aref (sb2-names b) i)))
(defmethod book-load-image ((b sharp-book) i)
  (let ((es (dotnet:invoke (aref (sb2-entries b) i) "OpenEntryStream"))
        (ms (dotnet:new "System.IO.MemoryStream")))
    (unwind-protect
         (progn (dotnet:invoke es "CopyTo" ms)
                (dotnet:invoke ms "set_Position" 0)
                (dotnet:static "Paalam.Imaging" "BitmapFromStream" ms))
      (dotnet:invoke es "Dispose")
      (dotnet:invoke ms "Dispose"))))
(defmethod book-close ((b sharp-book))
  (ignore-errors (dotnet:invoke (sb2-archive b) "Dispose")))

;;; pdf-book (PDFtoImage/PDFium 経由) -------------------------------
;;; 各ページを C# シム Paalam.PdfDoc でラスタライズして Avalonia Bitmap に。

(defclass pdf-book ()
  ((doc   :initarg :doc   :reader pdfb-doc)
   (count :initarg :count :reader pdfb-count)))

(defun make-pdf-book (path)
  (let* ((doc (dotnet:new "Paalam.PdfDoc" path))
         (n   (dotnet:invoke doc "get_PageCount")))
    (make-instance 'pdf-book :doc doc :count n)))

(defmethod book-page-count ((b pdf-book)) (pdfb-count b))
(defmethod book-page-name  ((b pdf-book) i) (format nil "p~d" (1+ i)))
(defmethod book-load-image ((b pdf-book) i) (dotnet:invoke (pdfb-doc b) "RenderPage" i))
(defmethod book-close      ((b pdf-book)) nil)

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
         ((and ext (member (string-downcase ext)
                           '("rar" "cbr" "7z" "cb7" "tar" "cbt") :test #'string=))
          (values (make-sharp-book path) 0))
         ((and ext (string= (string-downcase ext) "pdf"))
          (values (make-pdf-book path) 0))
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
(defparameter *recursive* t
  "フォルダを本として開くとき、サブフォルダの画像も再帰的に集める。")

;;; --- 設定の永続化 (s式 plist / OS の app data) ---------------------
(defparameter *settings*
  '((:page-direction    . *page-direction*)
    (:spread            . *spread*)
    (:first-page-single . *first-page-single*)
    (:recursive         . *recursive*))
  "保存対象の (設定キー . 特殊変数シンボル)。ここに足すだけで永続化対象になる。")

(defun %config-dir ()
  "per-user 設定ディレクトリ + paalam/。
Win=%APPDATA%, Linux=~/.config (XDG) は GetFolderPath(ApplicationData) が正しく返す。
macOS だけ GetFolderPath が ~/.config を返す .NET の癖があるので
Mac ネイティブの ~/Library/Application Support に寄せる (dotnet/runtime #40353)。"
  (let ((base
          (if (dotnet:static "System.OperatingSystem" "IsMacOS")
              (concatenate 'string
                           (dotnet:static "System.Environment" "GetFolderPath"
                                          (dotnet:static "System.Environment+SpecialFolder" "UserProfile"))
                           "/Library/Application Support")
              (dotnet:static "System.Environment" "GetFolderPath"
                             (dotnet:static "System.Environment+SpecialFolder" "ApplicationData")))))
    (concatenate 'string base "/paalam/")))

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

;;; ------------------------------------------------------------------
;;; 先読みローダー (#17): 単一ワーカースレッドが書庫アクセスを一手に担い、
;;; デコード済み Bitmap を cache。UI は cache を読むだけ。ロード完了は
;;; Dispatcher.UIThread.Post で UI へ通知して差し替え。NAS 上の書庫でも
;;; めくる前に裏で先読みするので快適。
;;; ------------------------------------------------------------------

(defparameter *cache-size* 16 "デコード済みページの最大保持数。")
(defparameter *prefetch-ahead* 2 "現在の見開きから何見開き先まで先読みするか。")

(defstruct loader
  book
  (cache (make-hash-table))           ; index -> Avalonia Bitmap
  (lock (make-lock "paalam-loader"))
  (cv (make-condition-variable))
  (queue '())                         ; ロード待ち index (優先順)
  (current 0)                         ; LRU の中心 (現在 index)
  (done nil)
  worker
  on-loaded)                          ; (lambda (index)) — UI へ通知

(defun loader-get (ld index)
  (with-lock-held ((loader-lock ld)) (gethash index (loader-cache ld))))

(defun %loader-evict (ld)
  (let ((cache (loader-cache ld)) (cur (loader-current ld)))
    (loop while (> (hash-table-count cache) *cache-size*) do
      (let ((far nil) (fard -1))
        (maphash (lambda (k v) (declare (ignore v))
                   (let ((d (abs (- k cur)))) (when (> d fard) (setf fard d far k))))
                 cache)
        (if far (remhash far cache) (return))))))

(defun loader-request (ld indices current)
  "indices を優先順でロード要求 (既キャッシュはスキップ)。current は LRU 中心。"
  (with-lock-held ((loader-lock ld))
    (setf (loader-current ld) current
          (loader-queue ld) (remove-if (lambda (i) (gethash i (loader-cache ld))) indices))
    (condition-notify (loader-cv ld))))

(defun %loader-loop (ld)
  (loop
    (let ((index nil))
      (with-lock-held ((loader-lock ld))
        (loop while (and (null (loader-queue ld)) (not (loader-done ld)))
              do (condition-wait (loader-cv ld) (loader-lock ld)))
        (when (and (loader-done ld) (null (loader-queue ld))) (return))
        (setf index (pop (loader-queue ld))))
      (when index
        (let ((bmp (ignore-errors (book-load-image (loader-book ld) index))))
          (when bmp
            (with-lock-held ((loader-lock ld))
              (setf (gethash index (loader-cache ld)) bmp)
              (%loader-evict ld))
            (ignore-errors (funcall (loader-on-loaded ld) index))))))))

(defun open-loader (book on-loaded)
  (let ((ld (make-loader :book book :on-loaded on-loaded)))
    (setf (loader-worker ld)
          (make-thread (lambda () (%loader-loop ld)) :name "paalam-loader"))
    ld))

(defun loader-stop (ld)
  (with-lock-held ((loader-lock ld))
    (setf (loader-done ld) t)
    (condition-notify (loader-cv ld))))

(defun %ui-post (thunk)
  "thunk を UI スレッドで実行 (ワーカー→UI マーシャリング)。"
  (let ((act (dotnet:make-delegate "System.Action" thunk)))
    (dotnet:invoke (dotnet:static "Avalonia.Threading.Dispatcher" "UIThread")
                   "Post" act
                   (dotnet:static "Avalonia.Threading.DispatcherPriority" "Background"))))

;;; viewer: book + 見開き単位 (spreads) + 現在位置 + 2 枚の Image
;;; spreads = 各見開きの page-index リスト (1 or 2 個) の vector。
;;; 単ページ/見開きを同じ仕組みで扱う (単ページ時は全要素 1 個)。
;;; zoom/tx/ty + scale-xf/trans-xf は stage の RenderTransform (ズーム/パン)。
;;; loader = 先読みローダー。
(defstruct viewer book spreads (sindex 0) window left-img right-img source loader
           viewport scale-xf trans-xf (zoom 1d0) (tx 0d0) (ty 0d0)
           (dragging nil) (lx 0d0) (ly 0d0))

(defun %reopen (vw)
  "source を開き直して book/loader/spreads を差し替える (再帰トグル等で使用)。"
  (multiple-value-bind (book start) (open-book (viewer-source vw))
    (ignore-errors (loader-stop (viewer-loader vw)))
    (ignore-errors (book-close (viewer-book vw)))
    (setf (viewer-book vw) book
          (viewer-loader vw) (open-loader
                              book
                              (lambda (index)
                                (%ui-post (lambda () (%on-page-loaded vw index)))))
          (viewer-spreads vw) (compute-spreads (book-page-count book))
          (viewer-sindex vw) (spread-index-of (viewer-spreads vw) start))
    (show-spread vw)))

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

(defun %page-bitmap (vw index) (and index (loader-get (viewer-loader vw) index)))

(defun %render-spread (vw)
  "現在の見開きを cache から描画 (未ロードは空欄。後で %on-page-loaded が埋める)。"
  (let* ((spreads (viewer-spreads vw)) (m (length spreads)))
    (when (plusp m)
      (let* ((si (viewer-sindex vw))
             (pages (aref spreads si))
             (li (viewer-left-img vw))
             (ri (viewer-right-img vw)))
        (if (= (length pages) 1)
            (progn
              (%place-image li (%page-bitmap vw (first pages)) 0 2 "Center")
              (%place-image ri nil 1 1 "Left"))
            (destructuring-bind (lo hi) pages
              ;; rtl(右開き): 下位 index=lo を右に、hi を左に。ltr は逆。
              (let ((left-page  (if (eq *page-direction* :rtl) hi lo))
                    (right-page (if (eq *page-direction* :rtl) lo hi)))
                (%place-image li (%page-bitmap vw left-page)  0 1 "Right")
                (%place-image ri (%page-bitmap vw right-page) 1 1 "Left"))))))))

(defun %load-order (vw)
  "現在の見開き + prefetch 分の page index を優先順 (現在見開きが先) で。"
  (let* ((spreads (viewer-spreads vw)) (m (length spreads)) (acc '()))
    (when (plusp m)
      (loop for k from 0 to *prefetch-ahead*
            for si = (+ (viewer-sindex vw) k)
            when (< si m)
            do (dolist (p (aref spreads si)) (push p acc))))
    (nreverse acc)))

(defun show-spread (vw)
  "見開き表示 (sindex クランプ)。必要ページをローダーに要求し cache から描画、
読み方向の次見開きも prefetch 要求する。"
  (let* ((spreads (viewer-spreads vw)) (m (length spreads)))
    (when (plusp m)
      (let ((si (max 0 (min (viewer-sindex vw) (1- m)))))
        (setf (viewer-sindex vw) si)
        (loader-request (viewer-loader vw) (%load-order vw) si)
        (%render-spread vw)
        (dotnet:invoke (viewer-window vw) "set_Title"
                       (%spread-title vw (aref spreads si)))
        (reset-view vw)))))

(defun %on-page-loaded (vw index)
  "ワーカーが index をロード完了 → 現在の見開きに含まれるなら cache から再描画
(zoom/pan は維持; reset-view しない)。"
  (let* ((spreads (viewer-spreads vw)) (m (length spreads)))
    (when (plusp m)
      (let ((pages (aref spreads (viewer-sindex vw))))
        (when (member index pages) (%render-spread vw))))))

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
        ((string= kn "R") (setf *recursive* (not *recursive*)) (save-settings) (%reopen vw))
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
                              :source (%resolve-path *target-path*)
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
                        (lambda (s e) (declare (ignore s e))
                          (ignore-errors (loader-stop (viewer-loader vw)))
                          (book-close book)))
      ;; 先読みローダーを起動 (vw を closure で参照するので vw 作成後)。
      (setf (viewer-loader vw)
            (open-loader book
                         (lambda (index)
                           (%ui-post (lambda () (%on-page-loaded vw index))))))
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
