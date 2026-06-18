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

;;; define-class (UI クラス emit) とスレッド/ロック/条件変数 (先読みローダー) の contrib。
;;; DotCL.Runtime 0.1.11 は contrib を出力に同梱するので require で解決できる。
;;; *package* = :paalam で require することで、define-class マクロが束縛する `self' が
;;; この下の define-class 本体 (:paalam) と同一シンボルになる。
(require :dotnet-class)
(require :dotcl-thread)
(use-package :dotcl-thread)

(defvar *debug* nil "PAALAM_DEBUG で有効化。先読みの詳細を debug.log に出す。")
(defun %dbg (fmt &rest args)
  (when *debug*
    (ignore-errors
      (with-open-file (s (concatenate 'string (%config-dir) "debug.log")
                         :direction :output :if-exists :append :if-does-not-exist :create)
        (format s "~a~%" (apply #'format nil fmt args))))))

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

;;; book-page-bytes: 素材掃き出し (#18) 用にページの生バイトと拡張子を返す。
;;; (values byte[] extension)。dir/zip/sharp は格納形式そのまま、pdf は PNG 化。
(defgeneric book-page-bytes (book index))

(defun %name-ext (name) (string-downcase (or (pathname-type name) "dat")))

(defmethod book-page-bytes ((b directory-book) i)
  (let ((path (aref (db-paths b) i)))
    (values (dotnet:static "System.IO.File" "ReadAllBytes" path) (%name-ext path))))

(defmethod book-page-bytes ((b zip-book) i)
  (let ((es (dotnet:invoke (aref (zb-entries b) i) "Open"))
        (ms (dotnet:new "System.IO.MemoryStream")))
    (unwind-protect
         (progn (dotnet:invoke es "CopyTo" ms)
                (values (dotnet:invoke ms "ToArray") (%name-ext (aref (zb-names b) i))))
      (dotnet:invoke es "Dispose") (dotnet:invoke ms "Dispose"))))

(defmethod book-page-bytes ((b sharp-book) i)
  (let ((es (dotnet:invoke (aref (sb2-entries b) i) "OpenEntryStream"))
        (ms (dotnet:new "System.IO.MemoryStream")))
    (unwind-protect
         (progn (dotnet:invoke es "CopyTo" ms)
                (values (dotnet:invoke ms "ToArray") (%name-ext (aref (sb2-names b) i))))
      (dotnet:invoke es "Dispose") (dotnet:invoke ms "Dispose"))))

(defmethod book-page-bytes ((b pdf-book) i)
  (values (dotnet:invoke (pdfb-doc b) "RenderPagePng" i) "png"))

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
(defparameter *fit-mode* :whole
  "フィットモード。:whole=全体(中央), :width=幅フィット(縦スクロール),
:height=高フィット(横スクロール)。実効スケール = フィット基準 × user zoom (#15)。")
(defparameter *background-color* "Black"
  "ウィンドウ背景 (= フィット時の余白/レターボックス) のブラシ名。Avalonia.Media.Brushes の名前 (#2)。")
(defparameter *interpolation* :high
  "拡大時の補間品質。:high=HighQuality (なめらか), :low=LowQuality (くっきり寄り) (#2)。")

;;; --- 設定の永続化 (s式 plist / OS の app data) ---------------------
(defparameter *settings*
  '((:page-direction    . *page-direction*)
    (:spread            . *spread*)
    (:first-page-single . *first-page-single*)
    (:recursive         . *recursive*)
    (:fit-mode          . *fit-mode*)
    (:background-color  . *background-color*)
    (:interpolation     . *interpolation*)
    (:cache-size        . *cache-size*)
    (:prefetch-ahead    . *prefetch-ahead*)
    (:loader-threads    . *loader-threads*))
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

;;; --- 履歴 + ブックマーク (#7) -------------------------------------
;;; 履歴: 本ごとに最後に見たページを覚え、再度開くとそこから再開。
;;; ブックマーク: ページにしおり (B 切替, ★ 表示, [ ] で前後のしおりへ)。
;;; どちらも s式で app data に永続化。横断ブラウズ UI は #6/#9。

(defun %state-file (name) (concatenate 'string (%config-dir) name))

(defun %read-state (name)
  (handler-case
      (let ((file (%state-file name)))
        (when (dotnet:static "System.IO.File" "Exists" file)
          (with-open-file (s file :direction :input)
            (let ((*read-eval* nil)) (read s nil nil)))))
    (error (e) (log-err name e) nil)))

(defun %write-state (name data)
  (handler-case
      (progn
        (dotnet:static "System.IO.Directory" "CreateDirectory" (%config-dir))
        (with-open-file (s (%state-file name) :direction :output
                           :if-exists :supersede :if-does-not-exist :create)
          (with-standard-io-syntax (prin1 data s) (terpri s))))
    (error (e) (log-err name e))))

;;; 履歴
(defparameter *history* nil "((path . page) ...) 最近順。")
(defparameter *history-limit* 200)
(defun load-history () (setf *history* (%read-state "history.lisp")))
(defun save-history () (%write-state "history.lisp" *history*))
(defun history-page (path) (cdr (assoc path *history* :test #'string-equal)))
(defun history-update (path page)
  (when path
    (setf *history* (cons (cons path page)
                          (remove path *history* :key #'car :test #'string-equal)))
    (when (> (length *history*) *history-limit*)
      (setf *history* (subseq *history* 0 *history-limit*)))))

;;; ブックマーク
(defparameter *bookmarks* nil "((path . page) ...)。")
(defun load-bookmarks () (setf *bookmarks* (%read-state "bookmarks.lisp")))
(defun save-bookmarks () (%write-state "bookmarks.lisp" *bookmarks*))
(defun bookmarked-p (path page)
  (and path (member-if (lambda (e) (and (string-equal (car e) path) (eql (cdr e) page)))
                       *bookmarks*)))
(defun toggle-bookmark (path page)
  (when path
    (if (bookmarked-p path page)
        (setf *bookmarks* (remove-if (lambda (e) (and (string-equal (car e) path)
                                                      (eql (cdr e) page)))
                                     *bookmarks*))
        (push (cons path page) *bookmarks*))
    (save-bookmarks)))
(defun book-bookmark-pages (path)
  (sort (loop for e in *bookmarks* when (string-equal (car e) path) collect (cdr e)) #'<))

;;; 素材クリップ (#18 Phase 1): ((source . page) ...) 追加順。zip 掃き出し用。
(defparameter *clip* nil)
;; サムネのメモリキャッシュ (source.page -> Bitmap)。ペインを開くたびに source から
;; 再取得しないため (NAS で効く)。loader の cache とは別物 (loader は evict で Dispose
;; するので共有しない)。
(defparameter *thumb-cache* (make-hash-table :test 'equal))
(defun %thumb-get (src page) (gethash (cons src page) *thumb-cache*))
(defun %thumb-put (src page bmp) (setf (gethash (cons src page) *thumb-cache*) bmp))
(defun load-clip () (setf *clip* (%read-state "clip.lisp")))
(defun save-clip () (%write-state "clip.lisp" *clip*))
(defun clip-add (source page)
  "素材に (source . page) を追加 (重複はスキップ)。"
  (when (and source
             (not (member-if (lambda (e) (and (string-equal (car e) source) (eql (cdr e) page)))
                             *clip*)))
    (setf *clip* (append *clip* (list (cons source page))))
    (save-clip)))
(defun clip-clear () (setf *clip* nil) (clrhash *thumb-cache*) (save-clip))

(defun %sanitize (s)
  (map 'string (lambda (c) (if (or (alphanumericp c) (member c '(#\- #\_ #\.))) c #\_))
       (or s "x")))

(defun %write-clip-zip (out clip)
  "CLIP の各ページを連番リネームして zip OUT に書き出す (各 source は開き直して読む)。
背景スレッドから呼んで UI を固めない。OUT を返す。"
  (when clip
    (ignore-errors (dotnet:load-assembly "System.IO.Compression"))
    (let* ((fs (dotnet:static "System.IO.File" "Create" out))
           (zip nil)
           (books (make-hash-table :test 'equal))
           (seq 0))
      (unwind-protect
           (progn
             (setf zip (dotnet:new "System.IO.Compression.ZipArchive" fs
                                   (dotnet:static "System.IO.Compression.ZipArchiveMode" "Create")))
             (dolist (item clip)
               (incf seq)
               (handler-case
                   (let* ((src (car item)) (page (cdr item))
                          (book (or (gethash src books)
                                    (setf (gethash src books) (open-book src)))))
                     (multiple-value-bind (bytes ext) (book-page-bytes book page)
                       (let* ((nm (format nil "~4,'0d_~a.~a" seq
                                          (%sanitize (pathname-name (book-page-name book page))) ext))
                              (entry (dotnet:invoke zip "CreateEntry" nm))
                              (es (dotnet:invoke entry "Open")))
                         (unwind-protect
                              (dotnet:invoke es "Write" bytes 0 (dotnet:invoke bytes "get_Length"))
                           (dotnet:invoke es "Dispose")))))
                 (error (e) (log-err "export-clip-item" e)))))
        (when zip (ignore-errors (dotnet:invoke zip "Dispose")))
        (ignore-errors (dotnet:invoke fs "Dispose"))
        (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (book-close v))) books))
      out)))

;;; 素材ペイン (#18 Phase 2): 集めたページをサムネ一覧で表示。サムネは背景
;;; スレッドで非同期ロードして後から差し込む (NAS でも UI を固めない)。

(defun %clip-remove-at (vw idx)
  "idx 番目の素材を取り除いてペイン更新。"
  (setf *clip* (loop for e in *clip* for i from 0 unless (= i idx) collect e))
  (save-clip)
  (%update-title vw)
  (%rebuild-clip-pane vw))

(defun %reorder-target (vw y)
  "clip-stack 内の Y 座標から挿入位置 (0..n) を求める (各 cell の中点で判定)。"
  (let* ((children (dotnet:invoke (viewer-clip-stack vw) "get_Children"))
         (n (dotnet:invoke children "get_Count")))
    (dotimes (i n n)
      (let* ((cell (dotnet:invoke children "get_Item" i))
             (b   (dotnet:invoke cell "get_Bounds"))
             (top (dotnet:invoke b "get_Y"))
             (h   (dotnet:invoke b "get_Height")))
        (when (< y (+ top (/ h 2.0d0))) (return-from %reorder-target i))))))

(defun %clip-reorder (vw from to)
  "from の素材を、元リスト基準の挿入位置 to (0..n) へ移してペイン更新。"
  (when (and from (/= from to) (>= from 0) (< from (length *clip*)))
    (let* ((item (nth from *clip*))
           (rest (loop for e in *clip* for i from 0 unless (= i from) collect e))
           (pos  (max 0 (min (if (> to from) (1- to) to) (length rest)))))
      (setf *clip* (append (subseq rest 0 pos) (list item) (subseq rest pos)))
      (save-clip)
      (%rebuild-clip-pane vw))))

;;; 並べ替えの視覚フィードバック (#20): 掴んだサムネを半透明化 + 挿入位置に青い線。
(defun %ensure-reorder-line (vw)
  "挿入位置インジケータ Border を用意 (使い回し)。"
  (or (viewer-reorder-line vw)
      (setf (viewer-reorder-line vw)
            (let ((b (dotnet:new "Avalonia.Controls.Border")))
              (dotnet:invoke b "set_Height" 3d0)
              (dotnet:invoke b "set_Margin" (dotnet:new "Avalonia.Thickness" 4d0 0d0 4d0 0d0))
              (dotnet:invoke b "set_Background" (dotnet:static "Avalonia.Media.Brushes" "DeepSkyBlue"))
              b))))

(defun %show-reorder-line (vw y)
  "clip-stack の Y にドラッグ中、挿入予定位置へインジケータを移動。"
  (let ((children (dotnet:invoke (viewer-clip-stack vw) "get_Children"))
        (line (%ensure-reorder-line vw)))
    ;; いったん外してから対象位置を計算 (line 自身が index をずらさないように)。
    (ignore-errors (dotnet:invoke children "Remove" line))
    (dotnet:invoke children "Insert" (%reorder-target vw y) line)))

(defun %clear-reorder-visual (vw)
  "挿入線を外し、掴んだサムネの不透明度を戻す (並べ替え終了/中断時)。"
  (let ((line (viewer-reorder-line vw)) (img (viewer-reorder-img vw)))
    (when line
      (ignore-errors
        (dotnet:invoke (dotnet:invoke (viewer-clip-stack vw) "get_Children") "Remove" line)))
    (when img (ignore-errors (dotnet:invoke img "set_Opacity" 1d0)))))

(defun %rebuild-clip-pane (vw)
  "clip-stack を作り直す。キャッシュ済みサムネは即表示、未キャッシュのみ背景で
ロードして %thumb-cache に貯めつつ差し込む (二度目以降は source 再取得なし)。"
  (let* ((stack (viewer-clip-stack vw))
         (children (dotnet:invoke stack "get_Children"))
         (snapshot (copy-list *clip*))
         (pending '()))                  ; (img . (source . page)) 未キャッシュ分
    (dotnet:invoke children "Clear")
    (loop for item in snapshot for i from 0 do
      (let ((img  (dotnet:new "Avalonia.Controls.Image"))
            (cell (dotnet:new "Avalonia.Controls.Grid"))   ; img + × を重ねる
            (xbtn (dotnet:new "Avalonia.Controls.Button"))
            (idx i)
            (cached (%thumb-get (car item) (cdr item))))
        (dotnet:invoke img "set_Width" 200d0)
        (dotnet:invoke img "set_Stretch" (dotnet:static "Avalonia.Media.Stretch" "Uniform"))
        ;; 左ドラッグで並べ替え (#18 Phase 3)。press で掴み、release 位置へ挿入。
        ;; ドラッグ中はサムネを半透明化 + 挿入位置に青線を出す (#20)。
        (dotnet:add-event img "PointerPressed"
          (lambda (s e) (declare (ignore s))
            (dotnet:invoke e "set_Handled" t)   ; ウィンドウのパンを起こさない
            (when (dotnet:invoke (dotnet:invoke (dotnet:invoke e "GetCurrentPoint" img)
                                                "get_Properties") "get_IsLeftButtonPressed")
              (setf (viewer-reorder-from vw) idx (viewer-reorder-img vw) img)
              (dotnet:invoke img "set_Opacity" 0.4d0)   ; 掴んでる感
              (%ensure-reorder-line vw)
              (ignore-errors (dotnet:invoke (dotnet:invoke e "get_Pointer") "Capture" img)))))
        (dotnet:add-event img "PointerMoved"
          (lambda (s e) (declare (ignore s))
            (when (viewer-reorder-from vw)
              (%show-reorder-line vw
                (dotnet:invoke (dotnet:invoke e "GetPosition" (viewer-clip-stack vw)) "get_Y")))))
        (dotnet:add-event img "PointerReleased"
          (lambda (s e) (declare (ignore s))
            (dotnet:invoke e "set_Handled" t)
            (let ((from (viewer-reorder-from vw)))
              (when from
                (let ((y (dotnet:invoke (dotnet:invoke e "GetPosition" (viewer-clip-stack vw)) "get_Y")))
                  (%clear-reorder-visual vw)   ; target 計算前に線を外す (index ずれ防止)
                  (let ((target (%reorder-target vw y)))
                    (setf (viewer-reorder-from vw) nil (viewer-reorder-img vw) nil)
                    (ignore-errors (dotnet:invoke (dotnet:invoke e "get_Pointer") "Capture" nil))
                    (%clip-reorder vw from target)))))))
        ;; × ボタン (右上)。赤背景+白太字で削除と分かるよう目立たせる。
        (dotnet:invoke xbtn "set_Content" "×")
        (dotnet:invoke xbtn "set_Foreground" (dotnet:static "Avalonia.Media.Brushes" "White"))
        (dotnet:invoke xbtn "set_Background" (dotnet:static "Avalonia.Media.Brushes" "Firebrick"))
        (dotnet:invoke xbtn "set_FontWeight" (dotnet:static "Avalonia.Media.FontWeight" "Bold"))
        (dotnet:invoke xbtn "set_FontSize" 14d0)
        (dotnet:invoke xbtn "set_HorizontalAlignment"
                       (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Right"))
        (dotnet:invoke xbtn "set_VerticalAlignment"
                       (dotnet:static "Avalonia.Layout.VerticalAlignment" "Top"))
        (dotnet:invoke xbtn "set_Padding" (dotnet:new "Avalonia.Thickness" 7d0 2d0))
        (dotnet:add-event xbtn "Click"
                          (lambda (s e) (declare (ignore s e)) (%clip-remove-at vw idx)))
        (dotnet:invoke cell "set_Margin" (dotnet:new "Avalonia.Thickness" 4d0))
        (let ((cc (dotnet:invoke cell "get_Children")))
          (dotnet:invoke cc "Add" img)
          (dotnet:invoke cc "Add" xbtn))
        (if cached
            (dotnet:invoke img "set_Source" cached)
            (push (cons img item) pending))
        (dotnet:invoke children "Add" cell)))
    (when pending
      (setf pending (nreverse pending))
      (make-thread
       (lambda ()
         (let ((books (make-hash-table :test 'equal)))
           (unwind-protect
                (dolist (pair pending)
                  (ignore-errors
                    (let* ((img (car pair)) (item (cdr pair))
                           (src (car item)) (page (cdr item))
                           (book (or (gethash src books)
                                     (setf (gethash src books) (open-book src))))
                           (bmp (book-load-image book page)))
                      (when bmp
                        (%thumb-put src page bmp)
                        (%ui-post (lambda () (dotnet:invoke img "set_Source" bmp)))))))
             (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (book-close v))) books))))
       :name "paalam-thumbs"))))

(defun %do-export (vw out)
  "out へ素材を背景スレッドで書き出し、完了後に UI を更新 (clip クリア)。"
  (let ((snapshot (copy-list *clip*)))
    (make-thread
     (lambda ()
       (ignore-errors (%write-clip-zip out snapshot))
       (%ui-post (lambda ()
                   (clip-clear) (%update-title vw) (%rebuild-clip-pane vw)
                   (dotnet:invoke (viewer-window vw) "set_Title"
                                  (format nil "掃き出し完了 → ~a" out)))))
     :name "paalam-export")))

(defun %pane-export (vw)
  "保存ダイアログで出力先を選び、背景で zip に掃き出す。"
  (when *clip*
    (let ((suggested (concatenate 'string "paalam-clip-"
                                  (dotnet:invoke (dotnet:static "System.DateTime" "Now")
                                                 "ToString" "yyyyMMdd-HHmmss")
                                  ".zip")))
      (dotnet:static "Paalam.DialogHelper" "PickSaveZip" (viewer-window vw) suggested
                     (dotnet:make-delegate "System.Action`1[System.String]"
                                           (lambda (out) (%do-export vw out)))))))

(defun %toggle-clip-pane (vw)
  (let* ((pane (viewer-clip-pane vw))
         (vis (dotnet:invoke pane "get_IsVisible")))
    (dotnet:invoke pane "set_IsVisible" (not vis))
    (unless vis (%rebuild-clip-pane vw))))

(defun %clip-drop (vw path)
  "ペインにドロップされた path を素材に追加 (画像=親フォルダ+そのページ、
フォルダ/書庫=全ページ)。"
  (handler-case
      (if (and (dotnet:static "System.IO.File" "Exists" path)
               (image-ext-p (pathname-type path)))
          ;; 画像: 親フォルダ (drive 込み) を source、その中での index を page。
          ;; directory-namestring は drive を落とすので Path.GetDirectoryName を使う。
          (let* ((dir (substitute #\/ #\\
                                  (dotnet:static "System.IO.Path" "GetDirectoryName" path)))
                 (files (%dir-image-files dir))
                 (idx (or (position (file-namestring path) files
                                    :key #'file-namestring :test #'string-equal)
                          0)))
            (clip-add dir idx))
          ;; フォルダ/書庫: 全ページ。
          (multiple-value-bind (book start) (open-book path)
            (declare (ignore start))
            (unwind-protect
                 (dotimes (p (book-page-count book)) (clip-add path p))
              (ignore-errors (book-close book)))))
    (error (e) (log-err "clip-drop" e)))
  (%update-title vw)
  (when (dotnet:invoke (viewer-clip-pane vw) "get_IsVisible") (%rebuild-clip-pane vw)))

;;; ------------------------------------------------------------------
;;; 先読みローダー (#17): 単一ワーカースレッドが書庫アクセスを一手に担い、
;;; デコード済み Bitmap を cache。UI は cache を読むだけ。ロード完了は
;;; Dispatcher.UIThread.Post で UI へ通知して差し替え。NAS 上の書庫でも
;;; めくる前に裏で先読みするので快適。
;;; ------------------------------------------------------------------

(defparameter *cache-size* 48
  "デコード済みページの最大保持数 (設定ファイルで調整可)。")
(defparameter *prefetch-ahead* 8
  "現在の見開きから何見開き先まで先読みするか (設定ファイルで調整可)。")
(defparameter *loader-threads* 4
  "並列ローダースレッド数。各スレッドが自前の書庫ハンドルで同時に読むので、
NAS の帯域/レイテンシを隠せる (設定ファイルで調整可)。")

(defstruct loader
  books                               ; 書庫ハンドルの vector (ワーカーが分担)
  (cache (make-hash-table))           ; index -> Avalonia Bitmap
  (inflight (make-hash-table))        ; 現在ロード中の index (二度読み防止)
  (lock (make-lock "paalam-loader"))
  (cv (make-condition-variable))
  (queue '())                         ; ロード待ち index (優先順)
  (current 0)                         ; LRU の中心 (現在ページ index)
  (done nil)
  workers                             ; ワーカースレッドのリスト
  on-loaded)                          ; (lambda (index)) — UI へ通知

(defun loader-get (ld index)
  (with-lock-held ((loader-lock ld)) (gethash index (loader-cache ld))))

(defun %loader-evict (ld)
  "cache が *cache-size* を超えたら現在ページから最も遠いものを捨てる。
Avalonia Bitmap は Skia の非マネージドメモリを抱えるので必ず Dispose する
(しないと ~100 ページで非マネージドメモリが膨らみ息切れする)。
追い出すのは常に最遠ページなので現在見開きの Bitmap は対象にならない。"
  (let ((cache (loader-cache ld)) (cur (loader-current ld)))
    (loop while (> (hash-table-count cache) *cache-size*) do
      (let ((far nil) (fard -1))
        (maphash (lambda (k v) (declare (ignore v))
                   (let ((d (abs (- k cur)))) (when (> d fard) (setf fard d far k))))
                 cache)
        (if far
            (let ((bmp (gethash far cache)))
              (remhash far cache)
              (ignore-errors (dotnet:invoke bmp "Dispose")))
            (return))))))

(defun loader-request (ld indices current)
  "indices を優先順でロード要求 (既キャッシュはスキップ)。current は LRU 中心。"
  (with-lock-held ((loader-lock ld))
    (setf (loader-current ld) current
          ;; 既にキャッシュ済み or ロード中のものは要求しない (二度読み防止)。
          (loader-queue ld) (remove-if (lambda (i) (or (gethash i (loader-cache ld))
                                                       (gethash i (loader-inflight ld))))
                                       indices))
    (%dbg "req cur=~a want=~a queued=~a cache=~a" current (length indices)
          (length (loader-queue ld)) (hash-table-count (loader-cache ld)))
    (condition-broadcast (loader-cv ld))))   ; 複数ワーカーを起こす

;;; book が並行ロード安全か。directory はファイル read が独立で安全 (1 ハンドル共有可)。
;;; zip/sharp/pdf は書庫/native ハンドルが非並行なので各ワーカーに別ハンドルを持たせる。
(defgeneric book-parallel-safe-p (book))
(defmethod book-parallel-safe-p ((b t)) nil)
(defmethod book-parallel-safe-p ((b directory-book)) t)

(defun %loader-loop (ld book)
  "ワーカー本体。BOOK はこのワーカー専用の書庫ハンドル (並列読み用)。"
  (loop
    (let ((index nil))
      (with-lock-held ((loader-lock ld))
        (loop while (and (null (loader-queue ld)) (not (loader-done ld)))
              do (condition-wait (loader-cv ld) (loader-lock ld)))
        ;; done なら待ち行列 (prefetch) を捨てて即終了。
        (when (loader-done ld) (return))
        (setf index (pop (loader-queue ld)))
        (when index (setf (gethash index (loader-inflight ld)) t)))   ; ロード中マーク
      (when index
        ;; ワーカーは決して死なせない。in-flight は必ず解除。
        (unwind-protect
            (handler-case
                (let ((bmp (book-load-image book index)))   ; 自前ハンドルで読む (並列)
                  (if bmp
                      (progn
                        (with-lock-held ((loader-lock ld))
                          (setf (gethash index (loader-cache ld)) bmp)
                          (%loader-evict ld))
                        (%dbg "cached ~a (cache=~a)" index (hash-table-count (loader-cache ld)))
                        (ignore-errors (funcall (loader-on-loaded ld) index)))
                      (%dbg "LOAD-NIL ~a" index)))
              (error (e) (%dbg "LOAD-ERR ~a : ~a" index e)))
          (with-lock-held ((loader-lock ld)) (remhash index (loader-inflight ld))))))))

(defun open-loader (path on-loaded)
  "PATH を *loader-threads* スレッドで並列ロードするローダーを起動。
並列安全な book は 1 ハンドル共有、そうでなければスレッド毎に別ハンドルを開く。"
  (let* ((n (max 1 *loader-threads*))
         (first (open-book path))
         (nh (if (book-parallel-safe-p first) 1 n))   ; ハンドル数
         (books (coerce (cons first (loop repeat (1- nh) collect (open-book path))) 'vector))
         (ld (make-loader :books books :on-loaded on-loaded)))
    (setf (loader-workers ld)
          (loop for i from 0 below n
                collect (let ((bk (aref books (mod i nh))))
                          (make-thread (lambda () (%loader-loop ld bk)) :name "paalam-loader"))))
    ld))

(defun loader-stop (ld)
  "全ワーカーを止め、cache の Bitmap と書庫ハンドルを解放する。"
  (with-lock-held ((loader-lock ld))
    (setf (loader-done ld) t)
    (condition-broadcast (loader-cv ld)))
  (dolist (w (loader-workers ld)) (ignore-errors (thread-join w)))   ; 全ワーカー終了待ち
  (with-lock-held ((loader-lock ld))
    (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (dotnet:invoke v "Dispose")))
             (loader-cache ld))
    (clrhash (loader-cache ld)))
  (loop for b across (loader-books ld) do (ignore-errors (book-close b))))   ; ハンドル解放

(defun %ui-post (thunk)
  "thunk を UI スレッドで実行 (ワーカー→UI マーシャリング)。"
  (let ((act (dotnet:make-delegate "System.Action" thunk)))
    (dotnet:invoke (dotnet:static "Avalonia.Threading.Dispatcher" "UIThread")
                   "Post" act
                   (dotnet:static "Avalonia.Threading.DispatcherPriority" "Normal"))))

;;; viewer: book + 見開き単位 (spreads) + 現在位置 + 2 枚の Image
;;; spreads = 各見開きの page-index リスト (1 or 2 個) の vector。
;;; 単ページ/見開きを同じ仕組みで扱う (単ページ時は全要素 1 個)。
;;; zoom/tx/ty + scale-xf/trans-xf は stage の RenderTransform (ズーム/パン)。
;;; loader = 先読みローダー。
(defstruct viewer book spreads (sindex 0) window left-img right-img source loader
           viewport scale-xf trans-xf (zoom 1d0) (tx 0d0) (ty 0d0)
           (fit-base 1d0)               ; フィット基準スケール (実効 = fit-base*zoom; #15)
           (content-w 0d0) (content-h 0d0)  ; 現在の見開きの content 寸法 (DIP; #15)
           (dragging nil) (lx 0d0) (ly 0d0)
           clip-pane clip-stack         ; 素材ペイン (#18 Phase 2)
           (reorder-from nil)           ; ドラッグ並べ替え中の元 index (Phase 3)
           (reorder-img nil)            ; ドラッグ中のサムネ (不透明度を戻す用; #20)
           (reorder-line nil)           ; 挿入位置インジケータ Border (#20)
           (dnd-overlay nil)            ; DnD ドロップ可否ハイライト枠 (#20)
           menu)                        ; メニューバー (#21; 全画面時に隠す)

(defun %reopen (vw)
  "source を開き直して book/loader/spreads を差し替える (再帰トグル等)。現在ページは維持。"
  (let ((keep (%cur-page vw)))
    (multiple-value-bind (book start) (open-book (viewer-source vw))
      (declare (ignore start))
      (ignore-errors (loader-stop (viewer-loader vw)))
      (ignore-errors (book-close (viewer-book vw)))
      (setf (viewer-book vw) book
            (viewer-loader vw) (open-loader
                                (viewer-source vw)
                                (lambda (index)
                                  (%ui-post (lambda () (%on-page-loaded vw index)))))
            (viewer-spreads vw) (compute-spreads (book-page-count book))
            (viewer-sindex vw) (spread-index-of (viewer-spreads vw)
                                                (min keep (max 0 (1- (book-page-count book))))))
      (show-spread vw))))

(defun open-path (vw path)
  "別の本 (DnD 等) を開いて差し替える。履歴があればそのページ、無ければ先頭から。
UI スレッドで呼ぶこと。"
  (handler-case
      (multiple-value-bind (book start) (open-book path)
        (ignore-errors (loader-stop (viewer-loader vw)))
        (ignore-errors (book-close (viewer-book vw)))
        (let* ((spreads (compute-spreads (book-page-count book)))
               (resume (history-page path))
               (page (if resume (min resume (max 0 (1- (book-page-count book)))) start)))
          (setf (viewer-source vw) path
                (viewer-book vw) book
                (viewer-loader vw) (open-loader
                                    path
                                    (lambda (i) (%ui-post (lambda () (%on-page-loaded vw i)))))
                (viewer-spreads vw) spreads
                (viewer-sindex vw) (spread-index-of spreads page))
          (show-spread vw)))
    (error (e) (log-err "open-path" e))))

(defun apply-transform (vw)
  ;; 実効スケール = フィット基準 × user zoom。translate は親 (viewport) 座標系の絶対オフセット。
  (let ((s (* (viewer-fit-base vw) (viewer-zoom vw))))
    (dotnet:invoke (viewer-scale-xf vw) "set_ScaleX" s)
    (dotnet:invoke (viewer-scale-xf vw) "set_ScaleY" s))
  (dotnet:invoke (viewer-trans-xf vw) "set_X" (viewer-tx vw))
  (dotnet:invoke (viewer-trans-xf vw) "set_Y" (viewer-ty vw)))

(defun %apply-background (vw)
  "ウィンドウ背景 (余白/レターボックス) を *background-color* に設定 (#2)。"
  (ignore-errors
    (dotnet:invoke (viewer-window vw) "set_Background"
                   (dotnet:static "Avalonia.Media.Brushes" *background-color*))))

(defun %apply-interpolation (vw)
  "拡大補間品質を左右画像に設定 (#2)。"
  (let ((mode (dotnet:static "Avalonia.Media.Imaging.BitmapInterpolationMode"
                             (if (eq *interpolation* :low) "LowQuality" "HighQuality"))))
    (dolist (img (list (viewer-left-img vw) (viewer-right-img vw)))
      (ignore-errors
        (dotnet:static "Avalonia.Media.RenderOptions" "SetBitmapInterpolationMode" img mode)))))

(defun %viewport-size (vw)
  "viewport の現在サイズ (DIP)。レイアウト前は 0。"
  (let ((b (dotnet:invoke (viewer-viewport vw) "get_Bounds")))
    (values (dotnet:invoke b "get_Width") (dotnet:invoke b "get_Height"))))

(defun %fit-base (cw ch vpw vph)
  "content (cw×ch) を viewport (vpw×vph) に合わせる基準スケール (モード別)。"
  (if (or (<= cw 0) (<= ch 0) (<= vpw 0) (<= vph 0))
      1d0
      (ecase *fit-mode*
        (:whole  (min (/ vpw cw) (/ vph ch)))
        (:width  (/ vpw cw))
        (:height (/ vph ch)))))

(defun %apply-fit (vw)
  "現在の content 寸法と viewport から基準スケールとアンカー (tx/ty) を決めて適用。
whole=中央寄せ、width/height=左上アンカー (はみ出しは pan でスクロール)。user zoom は保持。"
  (multiple-value-bind (vpw vph) (%viewport-size vw)
    (let* ((cw (viewer-content-w vw)) (ch (viewer-content-h vw))
           (base (%fit-base cw ch vpw vph))
           (s (* base (viewer-zoom vw))))
      (setf (viewer-fit-base vw) base)
      ;; 各軸: 収まるなら中央寄せ、はみ出すなら先頭(上/左)アンカー (はみ出し分は pan で
      ;; スクロール)。これで whole=中央、width=縦・height=横の自由軸が「余れば中央/
      ;; あふれれば先頭」になる。
      (setf (viewer-tx vw) (max 0d0 (/ (- vpw (* cw s)) 2d0))
            (viewer-ty vw) (max 0d0 (/ (- vph (* ch s)) 2d0)))
      (apply-transform vw))))

(defun reset-view (vw)
  "ズームを等倍に戻し、現モードでフィットし直す。"
  (setf (viewer-zoom vw) 1d0)
  (%apply-fit vw))

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

(defun %bmp-size (bmp)
  "Bitmap の DIP サイズ (values w h)。"
  (let ((s (dotnet:invoke bmp "get_Size")))
    (values (dotnet:invoke s "get_Width") (dotnet:invoke s "get_Height"))))

(defun %canvas-place (img bmp x y)
  "img に bmp を設定し、Canvas 上の (x,y) にネイティブ寸法で配置 (bmp=nil は非表示)。"
  (dotnet:invoke img "set_Source" bmp)
  (dotnet:invoke img "set_IsVisible" (and bmp t))
  (when bmp
    (dotnet:static "Avalonia.Controls.Canvas" "SetLeft" img (coerce x 'double-float))
    (dotnet:static "Avalonia.Controls.Canvas" "SetTop"  img (coerce y 'double-float))))

(defun %fit-label ()
  (case *fit-mode* (:width " [幅]") (:height " [高]") (t "")))

(defun %spread-title (vw pages)
  (let* ((book (viewer-book vw)) (n (book-page-count book))
         (path (viewer-source vw))
         (star (if (some (lambda (p) (bookmarked-p path p)) pages) "★ " ""))
         (clip (if *clip* (format nil " [素材~d]" (length *clip*)) ""))
         (fit  (%fit-label)))
    (if (= (length pages) 1)
        (format nil "~apaalam — ~a (~d/~d)~a~a"
                star (book-page-name book (first pages)) (1+ (first pages)) n clip fit)
        (destructuring-bind (lo hi) pages
          (format nil "~apaalam — ~a｜~a (~d-~d/~d)~a~a"
                  star (book-page-name book lo) (book-page-name book hi)
                  (1+ lo) (1+ hi) n clip fit)))))

(defun %page-bitmap (vw index) (and index (loader-get (viewer-loader vw) index)))

(defun %render-spread (vw)
  "現在の見開きを cache から Canvas に配置し content 寸法を更新 → フィット適用。
未ロードは空欄 (後で %on-page-loaded が埋める)。見開きは綴じ方向で左右を突き合わせ、
縦は背の高い方に中央寄せ。content 幅 = 左右幅の和、高さ = 高い方 (#15)。"
  (let* ((spreads (viewer-spreads vw)) (m (length spreads)))
    (when (plusp m)
      (let* ((si (viewer-sindex vw))
             (pages (aref spreads si))
             (li (viewer-left-img vw))
             (ri (viewer-right-img vw)))
        (if (= (length pages) 1)
            (let ((bmp (%page-bitmap vw (first pages))))
              (multiple-value-bind (w h) (if bmp (%bmp-size bmp) (values 0d0 0d0))
                (%canvas-place li bmp 0d0 0d0)
                (dotnet:invoke ri "set_IsVisible" nil)
                (setf (viewer-content-w vw) w (viewer-content-h vw) h)))
            (destructuring-bind (lo hi) pages
              ;; rtl(右開き): 下位 index=lo を右に、hi を左に。ltr は逆。
              (let* ((left-page  (if (eq *page-direction* :rtl) hi lo))
                     (right-page (if (eq *page-direction* :rtl) lo hi))
                     (lb (%page-bitmap vw left-page))
                     (rb (%page-bitmap vw right-page)))
                (multiple-value-bind (lw lh) (if lb (%bmp-size lb) (values 0d0 0d0))
                  (multiple-value-bind (rw rh) (if rb (%bmp-size rb) (values 0d0 0d0))
                    (let ((ch (max lh rh)))
                      (%canvas-place li lb 0d0 (/ (- ch lh) 2d0))
                      (%canvas-place ri rb lw  (/ (- ch rh) 2d0))
                      (setf (viewer-content-w vw) (+ lw rw)
                            (viewer-content-h vw) ch)))))))
        (%apply-fit vw)))))

(defun %update-title (vw)
  (let ((spreads (viewer-spreads vw)))
    (when (plusp (length spreads))
      (dotnet:invoke (viewer-window vw) "set_Title"
                     (%spread-title vw (aref spreads (viewer-sindex vw)))))))

(defun %goto-bookmark (vw dir)
  "現在の本のブックマークのうち、現在ページから dir 方向の最寄りページへ。"
  (let* ((cur (%cur-page vw))
         (pages (book-bookmark-pages (viewer-source vw)))
         (target (if (> dir 0)
                     (find-if (lambda (p) (> p cur)) pages)
                     (find-if (lambda (p) (< p cur)) (reverse pages)))))
    (when target
      (setf (viewer-sindex vw) (spread-index-of (viewer-spreads vw) target))
      (show-spread vw))))

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
        ;; LRU 中心は *ページ* index (見開き index ではない — 追い出し距離が狂うため)。
        (loader-request (viewer-loader vw) (%load-order vw) (%cur-page vw))
        (%dbg "show si=~a pages=~a hit=~a" si (aref spreads si)
              (mapcar (lambda (p) (if (%page-bitmap vw p) 1 0)) (aref spreads si)))
        (%render-spread vw)
        (%update-title vw)
        (history-update (viewer-source vw) (%cur-page vw))   ; in-memory。保存は終了時。
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

(defun %left-pressed-p (vw e)
  "今この瞬間、左ボタンが押されているか (PointerPoint.Properties)。"
  (dotnet:invoke (dotnet:invoke (dotnet:invoke e "GetCurrentPoint" (viewer-viewport vw))
                                "get_Properties")
                 "get_IsLeftButtonPressed"))

(defun %on-press (vw e)
  ;; パン開始は左ボタンのみ。メニュー等の押下で誤ってドラッグ状態にしない。
  (when (%left-pressed-p vw e)
    (multiple-value-bind (x y) (%pos-xy e (viewer-viewport vw))
      (setf (viewer-dragging vw) t (viewer-lx vw) x (viewer-ly vw) y))))

(defun %on-move (vw e)
  (when (viewer-dragging vw)
    ;; release を取りこぼして dragging が居残っても、ボタンが離れていれば自己修復で停止
    ;; (メニュー/別ウィンドウのダイアログで PointerReleased が main に返らない場合の対策)。
    (if (%left-pressed-p vw e)
        (multiple-value-bind (x y) (%pos-xy e (viewer-viewport vw))
          (incf (viewer-tx vw) (- x (viewer-lx vw)))
          (incf (viewer-ty vw) (- y (viewer-ly vw)))
          (setf (viewer-lx vw) x (viewer-ly vw) y)
          (apply-transform vw))
        (setf (viewer-dragging vw) nil))))

(defun %on-release (vw e) (declare (ignore e)) (setf (viewer-dragging vw) nil))

(defun %fullscreen-p (vw)
  (string= (dotnet:invoke (dotnet:invoke (viewer-window vw) "get_WindowState") "ToString")
           "FullScreen"))

(defun %set-fullscreen (vw on)
  (dotnet:invoke (viewer-window vw) "set_WindowState"
                 (dotnet:static "Avalonia.Controls.WindowState" (if on "FullScreen" "Normal")))
  ;; 全画面では没入のためメニューバーを隠す (DockPanel が root を全面に広げる)。
  (when (viewer-menu vw)
    (dotnet:invoke (viewer-menu vw) "set_IsVisible" (not on))))

(defun %quit (vw) (dotnet:invoke (viewer-window vw) "Close"))

;;; ------------------------------------------------------------------
;;; コマンドテーブル (#10): キー操作とメニューの単一の出どころ。
;;; 各コマンドは id・ラベル (メニュー表示)・既定キー (Avalonia Key.ToString 名)・
;;; ハンドラ (vw を取る関数 or シンボル) を持つ。キー dispatch もメニュー構築も
;;; ここから派生させ、ショートカット表記の二重管理を無くす。将来のキーバインド
;;; 編集 UI (段階2) はこのテーブルと *keymap* を編集する形にする。
;;; ------------------------------------------------------------------
(defstruct (command (:constructor make-command (id label keys handler)))
  id label keys handler)

;;; コマンド用のハンドラ (いずれも vw を 1 つ取る)。
(defun %next-spread (vw) (incf (viewer-sindex vw)) (show-spread vw))
(defun %prev-spread (vw) (decf (viewer-sindex vw)) (show-spread vw))
(defun %first-spread (vw) (setf (viewer-sindex vw) 0) (show-spread vw))
(defun %last-spread (vw)
  (setf (viewer-sindex vw) (1- (length (viewer-spreads vw)))) (show-spread vw))
(defun %reading-forward (vw)    ; ← : 綴じ方向に沿って読み進む
  (if (eq *page-direction* :rtl) (%next-spread vw) (%prev-spread vw)))
(defun %reading-backward (vw)   ; → : 読み戻る
  (if (eq *page-direction* :rtl) (%prev-spread vw) (%next-spread vw)))
(defun %toggle-fullscreen (vw) (%set-fullscreen vw (not (%fullscreen-p vw))))
(defun %escape (vw) (if (%fullscreen-p vw) (%set-fullscreen vw nil) (%quit vw)))
(defun %toggle-bookmark-current (vw)
  (toggle-bookmark (viewer-source vw) (%cur-page vw)) (%update-title vw))
(defun %clip-add-current (vw)
  "現在の見開きの全ページを素材に追加 (見開きは両ページ)。"
  (let ((sp (viewer-spreads vw)))
    (when (plusp (length sp))
      (dolist (p (aref sp (viewer-sindex vw))) (clip-add (viewer-source vw) p))))
  (%update-title vw)
  (when (dotnet:invoke (viewer-clip-pane vw) "get_IsVisible") (%rebuild-clip-pane vw)))

(defparameter *commands*
  ;; ラベルにキーは書かない (メニューが %keymap から付ける)。keys は既定割当。
  (list
   (make-command :open-file     "開く (ファイル)…" '()        (lambda (vw) (%open-file-dialog vw (viewer-window vw))))
   (make-command :open-folder   "開く (フォルダ)…" '()        (lambda (vw) (%open-folder-dialog vw (viewer-window vw))))
   (make-command :settings      "設定…"            '()        (lambda (vw) (%open-settings vw)))
   (make-command :quit          "終了"             '("Q")     '%quit)
   (make-command :next-spread   "次の見開き"       '("PageDown" "Next" "Space") '%next-spread)
   (make-command :prev-spread   "前の見開き"       '("PageUp" "Prior" "Back")   '%prev-spread)
   (make-command :reading-fwd   "読み進む"         '("Left")  '%reading-forward)
   (make-command :reading-bwd   "読み戻る"         '("Right") '%reading-backward)
   (make-command :first-spread  "先頭へ"           '("Home")  '%first-spread)
   (make-command :last-spread   "末尾へ"           '("End")   '%last-spread)
   (make-command :fullscreen    "全画面"           '("F11" "F") '%toggle-fullscreen)
   (make-command :spread        "見開き"           '("S")     '%toggle-spread)
   (make-command :direction     "綴じ方向"         '("D")     '%toggle-direction)
   (make-command :recursive     "再帰収集"         '("R")     '%toggle-recursive)
   (make-command :fit           "フィット"         '("D0" "NumPad0") 'reset-view)
   (make-command :fit-mode      "フィット切替"     '("W")     '%cycle-fit-mode)
   (make-command :clip-pane     "素材ペイン"       '("P")     '%toggle-clip-pane)
   (make-command :clip-add      "素材に追加"       '("A")     '%clip-add-current)
   (make-command :export        "掃き出し…"        '("E")     '%pane-export)
   (make-command :bookmark      "しおり切替"       '("B")     '%toggle-bookmark-current)
   (make-command :prev-bookmark "前のしおり"       '("OemOpenBrackets")  (lambda (vw) (%goto-bookmark vw -1)))
   (make-command :next-bookmark "次のしおり"       '("OemCloseBrackets") (lambda (vw) (%goto-bookmark vw  1)))
   (make-command :escape        "戻る / 終了"      '("Escape") '%escape))
  "全コマンド (定義順)。メニュー/キーバインド編集はこの順序を参照する。")

(defun %command (id) (find id *commands* :key #'command-id))

;;; キー割当 (#10 段階2): ユーザ上書きを *keybinds* に持ち、無いコマンドは既定キー。
;;; *keymap* (キー名→id) はそこから構築。keymap.lisp に永続化 (設定ウィンドウで編集)。
(defparameter *keybinds* nil
  "ユーザ上書き ((command-id . (key ...)) ...)。エントリの無いコマンドは既定キー。")
(defparameter *keymap* (make-hash-table :test 'equal)
  "キー名 (Avalonia Key.ToString) → コマンド id。*keybinds*+既定から構築。")

(defun %effective-keys (c)
  "コマンド C の実効キー (上書きがあればそれ、無ければ既定)。"
  (let ((o (assoc (command-id c) *keybinds*)))
    (if o (cdr o) (command-keys c))))

(defun rebuild-keymap ()
  "*keybinds*+既定から *keymap* を作り直す。"
  (clrhash *keymap*)
  (dolist (c *commands*)
    (dolist (k (%effective-keys c)) (setf (gethash k *keymap*) (command-id c)))))
(rebuild-keymap)

(defun load-keybinds ()
  (let ((d (%read-state "keymap.lisp"))) (when (listp d) (setf *keybinds* d)))
  (rebuild-keymap))
(defun save-keybinds () (%write-state "keymap.lisp" *keybinds*))

(defun %command-primary-key (id)
  "ID の代表キー (メニュー表示用; 既定キー順を優先、無ければ keymap から拾う)。"
  (let ((c (%command id)))
    (or (and c (find-if (lambda (k) (eq (gethash k *keymap*) id)) (command-keys c)))
        (loop for k being the hash-keys of *keymap* using (hash-value v)
              when (eq v id) return k))))

(defun %run-command (vw id)
  (let ((c (%command id)))
    (when c (funcall (command-handler c) vw))))

(defun %on-key (vw e)
  "Window KeyDown。キー名を *keymap* で引いて対応コマンドを実行 (#10)。"
  (let* ((kn (dotnet:invoke (dotnet:invoke e "get_Key") "ToString"))
         (id (gethash kn *keymap*)))
    (when id (%run-command vw id))))

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

;;; 表示トグル (キー / メニュー 共用)
(defun %toggle-spread (vw)
  (setf *spread* (not *spread*)) (%recompute vw (%cur-page vw)) (save-settings))
(defun %toggle-direction (vw)
  (setf *page-direction* (if (eq *page-direction* :rtl) :ltr :rtl))
  (show-spread vw) (save-settings))
(defun %toggle-recursive (vw)
  (setf *recursive* (not *recursive*)) (save-settings) (%reopen vw))
(defun %cycle-fit-mode (vw)
  "フィットモードを 全体→幅→高→全体 と巡回し、フィットし直す (#15)。"
  (setf *fit-mode* (ecase *fit-mode* (:whole :width) (:width :height) (:height :whole)))
  (save-settings)
  (reset-view vw)
  (%update-title vw))

(defun %open-file-dialog (vw win)
  (dotnet:static "Paalam.DialogHelper" "PickOpenBook" win
                 (dotnet:make-delegate "System.Action`1[System.String]"
                                       (lambda (p) (open-path vw p)))))
(defun %open-folder-dialog (vw win)
  (dotnet:static "Paalam.DialogHelper" "PickOpenFolder" win
                 (dotnet:make-delegate "System.Action`1[System.String]"
                                       (lambda (p) (open-path vw p)))))

;;; 設定ウィンドウ (#9): メニュー「設定…」から別ウィンドウで開く。OK で確定保存、
;;; キャンセルで破棄 (グローバル特殊変数は OK を押すまで一切触らない)。永続化基盤
;;; (*settings*/save-settings, #8) にそのまま乗せ、反映は変更内容に応じて再描画。
(defun %parse-int (s default min)
  "S を整数に。パース失敗 or MIN 未満なら DEFAULT を返す。"
  (let ((v (ignore-errors (parse-integer (string-trim '(#\Space #\Tab) s) :junk-allowed t))))
    (if (and v (>= v min)) v default)))

(defun %s-header (text)
  (let ((tb (dotnet:new "Avalonia.Controls.TextBlock")))
    (dotnet:invoke tb "set_Text" text)
    (dotnet:invoke tb "set_FontWeight" (dotnet:static "Avalonia.Media.FontWeight" "Bold"))
    (dotnet:invoke tb "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 10d0 0d0 4d0))
    tb))

(defun %s-row (label control)
  "[label(固定幅) ........ control] の横並び行。"
  (let ((row (dotnet:new "Avalonia.Controls.DockPanel"))
        (tb  (dotnet:new "Avalonia.Controls.TextBlock")))
    (dotnet:invoke tb "set_Text" label)
    (dotnet:invoke tb "set_Width" 170d0)
    (dotnet:invoke tb "set_VerticalAlignment"
                   (dotnet:static "Avalonia.Layout.VerticalAlignment" "Center"))
    (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" tb
                   (dotnet:static "Avalonia.Controls.Dock" "Left"))
    (dotnet:invoke row "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 3d0))
    (let ((c (dotnet:invoke row "get_Children")))
      (dotnet:invoke c "Add" tb)
      (dotnet:invoke c "Add" control))
    row))

(defun %set-check (cb on)
  "CheckBox.IsChecked (bool?) を確実に true/false に。Lisp nil をそのまま渡すと .NET null
= 中間(indeterminate)状態になるので、false はボックス化した System.Boolean(既定=false) を渡す。"
  (dotnet:invoke cb "set_IsChecked" (if on t (dotnet:new "System.Boolean"))))

(defun %s-checkbox (checked)
  (let ((cb (dotnet:new "Avalonia.Controls.CheckBox")))
    (dotnet:invoke cb "set_IsThreeState" nil)
    (%set-check cb checked) cb))

(defun %s-textbox (n)
  (let ((tb (dotnet:new "Avalonia.Controls.TextBox")))
    (dotnet:invoke tb "set_Text" (format nil "~d" n))
    (dotnet:invoke tb "set_Width" 80d0)
    (dotnet:invoke tb "set_HorizontalAlignment"
                   (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Left"))
    tb))

(defun %s-button (label click)
  (let ((b (dotnet:new "Avalonia.Controls.Button")))
    (dotnet:invoke b "set_Content" label)
    (dotnet:invoke b "set_MinWidth" 80d0)
    (dotnet:invoke b "set_Margin" (dotnet:new "Avalonia.Thickness" 6d0 0d0 0d0 0d0))
    (dotnet:add-event b "Click" (lambda (s e) (declare (ignore s e)) (funcall click)))
    b))

;;; 設定コンボの選択肢 (表示ラベル . 値)。
(defparameter *fit-choices*    '(("全体" . :whole) ("幅フィット" . :width) ("高さフィット" . :height)))
(defparameter *bg-choices*     '(("黒" . "Black") ("濃い灰" . "DimGray") ("灰" . "Gray") ("白" . "White")))
(defparameter *interp-choices* '(("高品質 (なめらか)" . :high) ("低品質 (くっきり寄り)" . :low)))

(defun %s-choice-combo (choices current &optional (test #'eql))
  "(ラベル . 値) の CHOICES から ComboBox を作り、CURRENT の項目を選択。"
  (let ((c (dotnet:new "Avalonia.Controls.ComboBox")))
    (let ((items (dotnet:invoke c "get_Items")))
      (dolist (ch choices) (dotnet:invoke items "Add" (car ch))))
    (dotnet:invoke c "set_SelectedIndex" (or (position current choices :key #'cdr :test test) 0))
    c))

(defun %s-combo-value (combo choices)
  (cdr (nth (max 0 (dotnet:invoke combo "get_SelectedIndex")) choices)))

;;; ファイル関連付け (#11, Windows): per-user (HKCU) なので管理者権限不要。ProgID を登録し
;;; 対応拡張子の OpenWithProgids に追加 (「プログラムから開く」に出る)。既定化は OS 仕様で
;;; 自動不可なので、登録後に「既定のアプリ」設定へ誘導する。レジストリ書き込みは Lisp から
;;; Microsoft.Win32.Registry を叩く (C# シムを増やさない方針)。
(defparameter *progid* "Paalam.Document")
(defparameter *assoc-extensions*
  (append *image-extensions* '("zip" "cbz" "rar" "cbr" "7z" "cb7" "tar" "cbt" "pdf"))
  "関連付け対象の拡張子 (paalam が開けるもの)。")

(defun %reg-set (path name value)
  "HKCU\\PATH のサブキー (無ければ作成) に (NAME -> VALUE) を書く。NAME=\"\" で既定値。"
  (let* ((cu (dotnet:static "Microsoft.Win32.Registry" "CurrentUser"))
         (k  (dotnet:invoke cu "CreateSubKey" path)))
    (unwind-protect (dotnet:invoke k "SetValue" name value)
      (ignore-errors (dotnet:invoke k "Close")))))

(defun %assoc-key (ext) (format nil "Software\\Classes\\.~a\\OpenWithProgids" ext))

(defun %ensure-progid ()
  "ProgID 本体 (開くコマンド + アイコン) を登録 (冪等)。exe は自分自身。"
  (let ((exe (dotnet:static "System.Environment" "ProcessPath")))
    (%reg-set (format nil "Software\\Classes\\~a" *progid*) "" "paalam 画像 / 書庫 / PDF")
    (%reg-set (format nil "Software\\Classes\\~a\\DefaultIcon" *progid*) "" (format nil "~a,0" exe))
    (%reg-set (format nil "Software\\Classes\\~a\\shell\\open\\command" *progid*) ""
              (format nil "\"~a\" \"%1\"" exe))))

(defun %assoc-registered-p (ext)
  "EXT の OpenWithProgids に *progid* が載っているか。"
  (handler-case
      (let* ((cu (dotnet:static "Microsoft.Win32.Registry" "CurrentUser"))
             (k  (dotnet:invoke cu "OpenSubKey" (%assoc-key ext))))
        (if k
            (unwind-protect (and (dotnet:invoke k "GetValue" *progid*) t)
              (ignore-errors (dotnet:invoke k "Close")))
            nil))
    (error () nil)))

(defun %assoc-register (exts)
  "EXTS を *progid* の OpenWithProgids 候補に追加 (per-user)。ProgID 本体も用意。"
  (ignore-errors (dotnet:load-assembly "Microsoft.Win32.Registry"))
  (%ensure-progid)
  (dolist (ext exts) (%reg-set (%assoc-key ext) *progid* "")))

(defun %assoc-unregister (exts)
  "EXTS の OpenWithProgids から *progid* を外す。"
  (ignore-errors (dotnet:load-assembly "Microsoft.Win32.Registry"))
  (dolist (ext exts)
    (ignore-errors
      (let* ((cu (dotnet:static "Microsoft.Win32.Registry" "CurrentUser"))
             (k  (dotnet:invoke cu "OpenSubKey" (%assoc-key ext) t)))   ; writable
        (when k
          (unwind-protect
               (ignore-errors (dotnet:invoke k "DeleteValue" *progid* nil)) ; throwOnMissing=false
            (ignore-errors (dotnet:invoke k "Close"))))))))

(defun %open-default-apps ()
  "Windows の「既定のアプリ」設定を開く (既定化はユーザ操作が要るため誘導)。"
  (ignore-errors
    (let ((psi (dotnet:new "System.Diagnostics.ProcessStartInfo")))
      (dotnet:invoke psi "set_FileName" "ms-settings:defaultapps")
      (dotnet:invoke psi "set_UseShellExecute" t)
      (dotnet:static "System.Diagnostics.Process" "Start" psi))))

(defun %settings-apply (vw cb-spread cb-fps cb-rec combo tb-cache tb-pf tb-th
                        fit-combo bg-combo interp-combo)
  "コントロールの現在値をグローバルへ反映し永続化、変更内容に応じて再描画する。"
  (let ((old-spread *spread*) (old-fps *first-page-single*)
        (old-rec *recursive*) (old-threads *loader-threads*))
    (setf *spread*            (and (dotnet:invoke cb-spread "get_IsChecked") t)
          *first-page-single* (and (dotnet:invoke cb-fps "get_IsChecked") t)
          *recursive*         (and (dotnet:invoke cb-rec "get_IsChecked") t)
          *page-direction*    (if (zerop (dotnet:invoke combo "get_SelectedIndex")) :rtl :ltr)
          *fit-mode*          (%s-combo-value fit-combo *fit-choices*)
          *background-color*  (%s-combo-value bg-combo *bg-choices*)
          *interpolation*     (%s-combo-value interp-combo *interp-choices*)
          *cache-size*        (%parse-int (dotnet:invoke tb-cache "get_Text") *cache-size* 1)
          *prefetch-ahead*    (%parse-int (dotnet:invoke tb-pf "get_Text") *prefetch-ahead* 0)
          *loader-threads*    (%parse-int (dotnet:invoke tb-th "get_Text") *loader-threads* 1))
    (save-settings)
    (%apply-background vw)       ; 背景色 (#2)
    (%apply-interpolation vw)    ; 補間品質 (#2)
    ;; 反映の重さ順: recursive/スレッド数は本を開き直す、見開き構成変化は再計算、
    ;; それ以外 (綴じ方向/フィット/背景/補間/キャッシュ/先読み) は現見開きを再描画。
    (cond ((or (not (eq old-rec *recursive*)) (/= old-threads *loader-threads*))
           (%reopen vw))
          ((or (not (eq old-spread *spread*)) (not (eq old-fps *first-page-single*)))
           (%recompute vw (%cur-page vw)))
          (t (show-spread vw)))))

(defparameter *modifier-keys*
  '("LeftShift" "RightShift" "LeftCtrl" "RightCtrl" "LeftAlt" "RightAlt" "LWin" "RWin")
  "単体では割当に使わない修飾キー (capture 中は無視して本命キーを待つ)。")

(defun %capture-key (owner on-key)
  "小窓を出し、次に押されたキー名を ON-KEY に渡して閉じる。Esc=キャンセル、修飾キー単体は無視。"
  (let ((w  (dotnet:new "Avalonia.Controls.Window"))
        (tb (dotnet:new "Avalonia.Controls.TextBlock")))
    (dotnet:invoke tb "set_Text" "割り当てるキーを押してください (Esc でキャンセル)")
    (dotnet:invoke tb "set_Margin" (dotnet:new "Avalonia.Thickness" 24d0))
    (dotnet:invoke w "set_Title" "キー割当")
    (dotnet:invoke w "set_SizeToContent"
                   (dotnet:static "Avalonia.Controls.SizeToContent" "WidthAndHeight"))
    (dotnet:invoke w "set_WindowStartupLocation"
                   (dotnet:static "Avalonia.Controls.WindowStartupLocation" "CenterOwner"))
    (dotnet:invoke w "set_Content" tb)
    (dotnet:add-event w "KeyDown"
      (lambda (s e) (declare (ignore s))
        (let ((kn (dotnet:invoke (dotnet:invoke e "get_Key") "ToString")))
          (unless (member kn *modifier-keys* :test #'string=)
            (dotnet:invoke e "set_Handled" t)
            (dotnet:invoke w "Close")
            (unless (string= kn "Escape") (funcall on-key kn))))))
    (dotnet:invoke w "ShowDialog" owner)))

(defun %open-settings (vw)
  ;; 左に折りたたみツリー (カテゴリ)、右に該当ページ。選択で右を差し替える master/detail。
  ;; OK/キャンセルは全ページ共通 (コントロールは非表示でも値を保持するので OK で読める)。
  (let* ((owner (viewer-window vw))
         (swin  (dotnet:new "Avalonia.Controls.Window"))
         (combo (dotnet:new "Avalonia.Controls.ComboBox"))
         (cb-spread (%s-checkbox *spread*))
         (cb-fps    (%s-checkbox *first-page-single*))
         (cb-rec    (%s-checkbox *recursive*))
         (tb-cache  (%s-textbox *cache-size*))
         (tb-pf     (%s-textbox *prefetch-ahead*))
         (tb-th     (%s-textbox *loader-threads*))
         (fit-combo    (%s-choice-combo *fit-choices* *fit-mode*))
         (bg-combo     (%s-choice-combo *bg-choices* *background-color* #'string=))
         (interp-combo (%s-choice-combo *interp-choices* *interpolation*))
         ;; キー割当の作業用コピー。(car kbox) が作業リスト。クロージャからは
         ;; (setf (car kbox) ..) で place を書き換える (let* 変数の再束縛は dotcl が不正 IL)。
         (kbox (list (let ((acc '()))
                       (dolist (c *commands*)
                         (dolist (k (%effective-keys c)) (push (cons k (command-id c)) acc)))
                       (nreverse acc))))
         (kb-stack (dotnet:new "Avalonia.Controls.StackPanel"))
         (content  (dotnet:new "Avalonia.Controls.ContentControl"))
         (win-p (dotnet:static "System.OperatingSystem" "IsWindows"))  ; 関連付けは Windows のみ (#11)
         (assoc-status (dotnet:new "Avalonia.Controls.TextBlock")))
    (let ((items (dotnet:invoke combo "get_Items")))
      (dotnet:invoke items "Add" "右開き (RTL)")
      (dotnet:invoke items "Add" "左開き (LTR)"))
    (dotnet:invoke combo "set_SelectedIndex" (if (eq *page-direction* :rtl) 0 1))
    (labels
        ((add-binding (k)
           (unless (assoc k (car kbox) :test #'string=)
             (setf (car kbox) (cons (cons k (command-id (car *commands*))) (car kbox))))
           (refresh))
         (remove-binding (pair) (setf (car kbox) (remove pair (car kbox))) (refresh))
         (kb-row (pair)
           (let ((row  (dotnet:new "Avalonia.Controls.DockPanel"))
                 (klbl (dotnet:new "Avalonia.Controls.TextBlock"))
                 (cbox (dotnet:new "Avalonia.Controls.ComboBox"))
                 (bdel (dotnet:new "Avalonia.Controls.Button")))
             (dotnet:invoke klbl "set_Text" (%key-label (car pair)))
             (dotnet:invoke klbl "set_Width" 90d0)
             (dotnet:invoke klbl "set_VerticalAlignment"
                            (dotnet:static "Avalonia.Layout.VerticalAlignment" "Center"))
             (let ((items (dotnet:invoke cbox "get_Items")))
               (dolist (c *commands*) (dotnet:invoke items "Add" (command-label c))))
             (dotnet:invoke cbox "set_SelectedIndex"
                            (or (position (cdr pair) *commands* :key #'command-id) 0))
             (dotnet:add-event cbox "SelectionChanged"
               (lambda (s e) (declare (ignore s e))
                 (let ((i (dotnet:invoke cbox "get_SelectedIndex")))
                   (when (>= i 0) (setf (cdr pair) (command-id (nth i *commands*)))))))
             (dotnet:invoke bdel "set_Content" "×")
             (dotnet:invoke bdel "set_Margin" (dotnet:new "Avalonia.Thickness" 6d0 0d0 0d0 0d0))
             (dotnet:add-event bdel "Click"
               (lambda (s e) (declare (ignore s e)) (remove-binding pair)))
             (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" klbl
                            (dotnet:static "Avalonia.Controls.Dock" "Left"))
             (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" bdel
                            (dotnet:static "Avalonia.Controls.Dock" "Right"))
             (dotnet:invoke row "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 2d0))
             (let ((rc (dotnet:invoke row "get_Children")))
               (dotnet:invoke rc "Add" klbl)
               (dotnet:invoke rc "Add" bdel)
               (dotnet:invoke rc "Add" cbox))
             row))
         (refresh ()
           (let ((ch (dotnet:invoke kb-stack "get_Children")))
             (dotnet:invoke ch "Clear")
             (dolist (pair (sort (copy-list (car kbox)) #'string< :key #'car))
               (dotnet:invoke ch "Add" (kb-row pair)))))
         (page (rows)
           "rows を縦に並べた設定ページ (StackPanel)。"
           (let ((p (dotnet:new "Avalonia.Controls.StackPanel")))
             (dotnet:invoke p "set_Margin" (dotnet:new "Avalonia.Thickness" 16d0))
             (let ((ch (dotnet:invoke p "get_Children")))
               (dolist (r rows) (dotnet:invoke ch "Add" r)))
             p))
         (tvi (header &optional children)
           "TreeViewItem を作る (children があれば展開済みの親ノード)。"
           (let ((it (dotnet:new "Avalonia.Controls.TreeViewItem")))
             (dotnet:invoke it "set_Header" header)
             (when children
               (dotnet:invoke it "set_IsExpanded" t)
               (let ((items (dotnet:invoke it "get_Items")))
                 (dolist (c children) (dotnet:invoke items "Add" c))))
             it))
         (assoc-page ()
           "関連付けページ (#11): 拡張子ごとチェックボックス + 登録/登録解除 (Windows)。"
           (let ((wrap  (dotnet:new "Avalonia.Controls.WrapPanel"))
                 (boxes (mapcar (lambda (ext)
                                  (let ((cb (dotnet:new "Avalonia.Controls.CheckBox")))
                                    (dotnet:invoke cb "set_Content" (format nil ".~a" ext))
                                    (dotnet:invoke cb "set_Width" 92d0)
                                    (dotnet:invoke cb "set_IsThreeState" nil)
                                    (%set-check cb (and win-p (%assoc-registered-p ext)))
                                    (cons ext cb)))
                                *assoc-extensions*)))
             (dotnet:invoke wrap "set_Width" 380d0)
             (dolist (b boxes) (dotnet:invoke (dotnet:invoke wrap "get_Children") "Add" (cdr b)))
             (labels ((checked-exts ()
                        (let ((acc '()))
                          (dolist (b boxes) (when (dotnet:invoke (cdr b) "get_IsChecked") (push (car b) acc)))
                          (nreverse acc)))
                      (set-all (v) (dolist (b boxes) (%set-check (cdr b) v)))
                      (sync ()  ; レジストリの実状態をチェックに反映
                        (dolist (b boxes)
                          (%set-check (cdr b) (%assoc-registered-p (car b))))))
               (let ((row (dotnet:new "Avalonia.Controls.StackPanel")))
                 (dotnet:invoke row "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
                 (let ((rc (dotnet:invoke row "get_Children")))
                   (dotnet:invoke rc "Add" (%s-button "全選択" (lambda () (set-all t))))
                   (dotnet:invoke rc "Add" (%s-button "全解除" (lambda () (set-all nil))))
                   (dotnet:invoke rc "Add"
                     (%s-button "登録"
                       (lambda ()
                         (let ((es (checked-exts)))
                           (handler-case
                               (progn (%assoc-register es) (sync)
                                      (dotnet:invoke assoc-status "set_Text"
                                        (format nil "~d 件を「プログラムから開く」候補に登録しました。" (length es))))
                             (error (e) (dotnet:invoke assoc-status "set_Text" (format nil "失敗: ~a" e))))))))
                   (dotnet:invoke rc "Add"
                     (%s-button "登録解除"
                       (lambda ()
                         (handler-case
                             (progn (%assoc-unregister (checked-exts)) (sync)
                                    (dotnet:invoke assoc-status "set_Text" "選択分の関連付けを解除。"))
                           (error (e) (dotnet:invoke assoc-status "set_Text" (format nil "失敗: ~a" e))))))))
                 (let ((desc (dotnet:new "Avalonia.Controls.TextBlock")))
                   (dotnet:invoke desc "set_Text"
                     "関連付ける拡張子を選び「登録」(per-user・管理者権限不要)。候補に載るだけで既定は変わらないので、既定にしたい型は『既定のアプリ』で paalam を選ぶ。")
                   (dotnet:invoke desc "set_TextWrapping" (dotnet:static "Avalonia.Media.TextWrapping" "Wrap"))
                   (dotnet:invoke desc "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 0d0 8d0))
                   ;; 既定化は任意。登録時に自動で開かず、押したい人だけ Windows の設定へ。
                   (let ((defbtn (%s-button "既定のアプリ設定を開く…" (lambda () (%open-default-apps)))))
                     (dotnet:invoke defbtn "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 8d0 0d0 0d0))
                     (dotnet:invoke defbtn "set_HorizontalAlignment"
                                    (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Left"))
                     (page (list (%s-header "関連付け (Windows / 管理者権限不要)")
                                 desc wrap row defbtn assoc-status)))))))))
      (refresh)
      (let* ((scr (dotnet:new "Avalonia.Controls.ScrollViewer"))
             (addbtn (%s-button "＋ キーを追加"
                                (lambda () (%capture-key swin (lambda (k) (add-binding k))))))
             (panel-layout
               (page (list (%s-header "レイアウト")
                           (%s-row "綴じ方向" combo)
                           (%s-row "見開き" cb-spread)
                           (%s-row "表紙を単独表示" cb-fps)
                           (%s-row "サブフォルダも再帰収集" cb-rec)
                           (%s-row "フィット" fit-combo))))
             (panel-quality
               (page (list (%s-header "画質")
                           (%s-row "背景色" bg-combo)
                           (%s-row "拡大補間" interp-combo))))
             (panel-perf
               (page (list (%s-header "先読み")
                           (%s-row "キャッシュ最大ページ数" tb-cache)
                           (%s-row "先読み見開き数" tb-pf)
                           (%s-row "ローダースレッド数" tb-th))))
             (panel-keys
               (progn
                 (dotnet:invoke scr "set_MaxHeight" 360d0)
                 (dotnet:invoke scr "set_Content" kb-stack)
                 (page (list (%s-header "キー割当 (キー → 機能。1 キー 1 機能、複数可)")
                             scr addbtn))))
             (panel-assoc (assoc-page))
             (pages (append (list (cons "レイアウト" panel-layout)
                                  (cons "画質"      panel-quality)
                                  (cons "先読み"    panel-perf)
                                  (cons "キー割当"  panel-keys))
                            (when win-p (list (cons "関連付け" panel-assoc)))))
             (tree (dotnet:new "Avalonia.Controls.TreeView"))
             (leaf-layout  (tvi "レイアウト"))
             (leaf-quality (tvi "画質"))
             (leaf-perf    (tvi "先読み"))
             (leaf-keys    (tvi "キー割当"))
             (leaf-assoc   (tvi "関連付け"))
             (g-view (tvi "表示" (list leaf-layout leaf-quality)))
             (g-perf (tvi "性能" (list leaf-perf)))
             (g-op   (tvi "操作" (list leaf-keys)))
             (g-system (tvi "システム" (list leaf-assoc)))
             (root (dotnet:new "Avalonia.Controls.DockPanel"))
             (grid (dotnet:new "Avalonia.Controls.Grid"))
             (btns (dotnet:new "Avalonia.Controls.StackPanel")))
        ;; 左ツリー (折りたたみ可能なカテゴリ)。選択された葉の header からページを引いて右へ。
        (let ((items (dotnet:invoke tree "get_Items")))
          (dotnet:invoke items "Add" g-view)
          (dotnet:invoke items "Add" g-perf)
          (dotnet:invoke items "Add" g-op)
          (when win-p (dotnet:invoke items "Add" g-system)))   ; 関連付けは Windows のみ
        (dotnet:add-event tree "SelectionChanged"
          (lambda (s e) (declare (ignore s e))
            (let ((item (dotnet:invoke tree "get_SelectedItem")))
              (when item
                (let ((p (cdr (assoc (dotnet:invoke item "get_Header") pages :test #'equal))))
                  (when p (dotnet:invoke content "set_Content" p)))))))
        (dotnet:invoke content "set_Content" panel-layout)   ; 初期ページ
        (dotnet:invoke leaf-layout "set_IsSelected" t)
        ;; 2 列 grid: [tree(180) | content(*)]
        (let ((cols (dotnet:invoke grid "get_ColumnDefinitions"))
              (c0 (dotnet:new "Avalonia.Controls.ColumnDefinition")))
          (dotnet:invoke c0 "set_Width" (dotnet:new "Avalonia.Controls.GridLength" 180d0))
          (dotnet:invoke cols "Add" c0)
          (dotnet:invoke cols "Add" (dotnet:new "Avalonia.Controls.ColumnDefinition")))
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" tree 0)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" content 1)
        (let ((gc (dotnet:invoke grid "get_Children")))
          (dotnet:invoke gc "Add" tree)
          (dotnet:invoke gc "Add" content))
        ;; OK/キャンセル (下部共通)
        (dotnet:invoke btns "set_Orientation"
                       (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
        (dotnet:invoke btns "set_HorizontalAlignment"
                       (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Right"))
        (dotnet:invoke btns "set_Margin" (dotnet:new "Avalonia.Thickness" 12d0))
        (let ((bc (dotnet:invoke btns "get_Children")))
          (dotnet:invoke bc "Add" (%s-button "キャンセル" (lambda () (dotnet:invoke swin "Close"))))
          (dotnet:invoke bc "Add"
            (%s-button "OK"
              (lambda ()
                (%settings-apply vw cb-spread cb-fps cb-rec combo tb-cache tb-pf tb-th
                                 fit-combo bg-combo interp-combo)
                ;; キー指向の作業リストを機能ごとにまとめ直して確定 (削除は空リストで反映)。
                (setf *keybinds*
                      (mapcar (lambda (c)
                                (let ((ks '()))
                                  (dolist (pair (car kbox))
                                    (when (eq (cdr pair) (command-id c)) (push (car pair) ks)))
                                  (cons (command-id c) (nreverse ks))))
                              *commands*))
                (rebuild-keymap)
                (save-keybinds)
                (dotnet:invoke swin "Close")))))
        ;; root: [btns(下) | grid(残り)]
        (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" btns
                       (dotnet:static "Avalonia.Controls.Dock" "Bottom"))
        (let ((rc (dotnet:invoke root "get_Children")))
          (dotnet:invoke rc "Add" btns)
          (dotnet:invoke rc "Add" grid))
        (dotnet:invoke swin "set_Title" "設定")
        (dotnet:invoke swin "set_Width" 640d0)
        (dotnet:invoke swin "set_Height" 520d0)
        (dotnet:invoke swin "set_WindowStartupLocation"
                       (dotnet:static "Avalonia.Controls.WindowStartupLocation" "CenterOwner"))
        (dotnet:invoke swin "set_Content" root)
        (dotnet:invoke swin "ShowDialog" owner)))))

;;; メニューバー (#21)
;;; Menu/MenuItem はキーボードフォーカスを保持する。クリック後フォーカスが
;;; メニューに残ると、矢印/Space/Esc 等がメニューのナビに食われて viewer の
;;; KeyDown まで届かない。そこでクリック後は必ずビューポートへフォーカスを戻す。
(defun %focus-viewport (vw)
  (ignore-errors (dotnet:invoke (viewer-viewport vw) "Focus")))

(defun %menu-item (vw header &optional click)
  (let ((mi (dotnet:new "Avalonia.Controls.MenuItem")))
    (dotnet:invoke mi "set_Header" header)
    (when click
      (dotnet:add-event mi "Click"
        (lambda (s e) (declare (ignore s e)) (funcall click) (%focus-viewport vw))))
    mi))
(defun %menu-add (parent item) (dotnet:invoke (dotnet:invoke parent "get_Items") "Add" item))

(defun %key-label (k)
  "Avalonia Key 名をメニュー表示用に整える ([ ] 0 等)。"
  (cond ((null k) nil)
        ((or (string= k "D0") (string= k "NumPad0")) "0")
        ((string= k "OemOpenBrackets") "[")
        ((string= k "OemCloseBrackets") "]")
        (t k)))

(defun %menu-cmd (vw parent id)
  "コマンド ID をメニュー項目として PARENT に追加 (ラベル + 代表キーを表示)。"
  (let* ((c (%command id))
         (k (%key-label (%command-primary-key id)))
         (header (if k (format nil "~a (~a)" (command-label c) k) (command-label c))))
    (%menu-add parent (%menu-item vw header (lambda () (%run-command vw id))))))

(defun %build-menu (vw win)
  (declare (ignore win))   ; 開く系コマンドは (viewer-window vw) を owner に使う
  (let ((menu   (dotnet:new "Avalonia.Controls.Menu"))
        (m-file (%menu-item vw "ファイル"))
        (m-view (%menu-item vw "表示"))
        (m-clip (%menu-item vw "素材")))
    (dolist (id '(:open-file :open-folder :settings :quit)) (%menu-cmd vw m-file id))
    (dolist (id '(:fullscreen :spread :direction :recursive :fit :fit-mode :clip-pane)) (%menu-cmd vw m-view id))
    (dolist (id '(:export)) (%menu-cmd vw m-clip id))
    (let ((items (dotnet:invoke menu "get_Items")))
      (dotnet:invoke items "Add" m-file)
      (dotnet:invoke items "Add" m-view)
      (dotnet:invoke items "Add" m-clip))
    menu))

(defun build-main-window ()
  "現在の *target-path* の本を開き、見開き対応の Avalonia Window を返す
(呼び出しスレッド上で構築)。2 列 Grid に 2 枚の Image を中央で突き合わせる。"
  (multiple-value-bind (book start) (open-book (%resolve-path *target-path*))
    (let* ((win   (dotnet:new "Avalonia.Controls.Window"))
           (vport (dotnet:new "Avalonia.Controls.Border"))   ; クリップ用ビューポート
           (stage (dotnet:new "Avalonia.Controls.Canvas"))   ; 見開き stage (座標は自前計算; #15)
           (li    (dotnet:new "Avalonia.Controls.Image"))
           (ri    (dotnet:new "Avalonia.Controls.Image"))
           (scale (dotnet:new "Avalonia.Media.ScaleTransform"))
           (trans (dotnet:new "Avalonia.Media.TranslateTransform"))
           (grp   (dotnet:new "Avalonia.Media.TransformGroup"))
           (spreads (compute-spreads (book-page-count book)))
           (path  (%resolve-path *target-path*))
           ;; 履歴があればそのページから再開 (#7)。
           (start2 (let ((r (history-page path)))
                     (if r (max 0 (min r (1- (book-page-count book)))) start)))
           (vw   (make-viewer :book book :spreads spreads
                              :sindex (spread-index-of spreads start2)
                              :source path
                              :window win :left-img li :right-img ri
                              :viewport vport :scale-xf scale :trans-xf trans)))
      ;; Canvas 上にネイティブ寸法で配置 (座標は %render-spread が計算)。Stretch=None。
      (dolist (im (list li ri))
        (dotnet:invoke im "set_Stretch" (dotnet:static "Avalonia.Media.Stretch" "None")))
      (let ((children (dotnet:invoke stage "get_Children")))
        (dotnet:invoke children "Add" li)
        (dotnet:invoke children "Add" ri))
      ;; stage に RenderTransform (実効スケール = フィット基準×zoom + パン)。原点 = 左上、
      ;; これで screen = scale*canvas座標 + translate と単純合成できる。
      (let ((gc (dotnet:invoke grp "get_Children")))
        (dotnet:invoke gc "Add" scale)
        (dotnet:invoke gc "Add" trans))
      (dotnet:invoke stage "set_RenderTransform" grp)
      (dotnet:invoke stage "set_RenderTransformOrigin"
                     (dotnet:static "Avalonia.RelativePoint" "TopLeft"))
      ;; viewport (clip) > stage
      ;; Focusable にしておき、起動時/メニュー操作後にここへフォーカスを置くことで
      ;; 矢印等のキーが必ず Window の KeyDown まで届く (メニューに吸われない)。
      (dotnet:invoke vport "set_Focusable" t)
      (dotnet:invoke vport "set_ClipToBounds" t)
      (dotnet:invoke vport "set_Child" stage)
      ;; リサイズで基準スケール/アンカーを再計算 (#15)。
      (dotnet:add-event vport "SizeChanged"
                        (lambda (s e) (declare (ignore s e)) (%apply-fit vw)))
      ;; root: [viewport* | 素材ペイン(Auto, 既定非表示)] (#18 Phase 2)
      (let* ((root  (dotnet:new "Avalonia.Controls.Grid"))
             (pane  (dotnet:new "Avalonia.Controls.DockPanel"))
             (ebtn  (dotnet:new "Avalonia.Controls.Button"))
             (scr   (dotnet:new "Avalonia.Controls.ScrollViewer"))
             (stack (dotnet:new "Avalonia.Controls.StackPanel"))
             (overlay (dotnet:new "Avalonia.Controls.Border")))   ; DnD ドロップ可否ハイライト枠 (#20)
        (let ((cols (dotnet:invoke root "get_ColumnDefinitions"))
              (cd   (dotnet:new "Avalonia.Controls.ColumnDefinition")))
          (dotnet:invoke cols "Add" (dotnet:new "Avalonia.Controls.ColumnDefinition")) ; star
          (dotnet:invoke cd "set_Width" (dotnet:static "Avalonia.Controls.GridLength" "Auto"))
          (dotnet:invoke cols "Add" cd))
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" vport 0)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" pane  1)
        (dotnet:invoke pane "set_Width" 220d0)
        (dotnet:invoke pane "set_IsVisible" nil)
        (dotnet:invoke pane "set_Background" (dotnet:static "Avalonia.Media.Brushes" "DimGray"))
        ;; DnD ハイライト枠: 全面を覆う太枠 (中身は透明)。当たり判定を切ってドロップを邪魔しない。
        (dotnet:invoke overlay "set_BorderBrush" (dotnet:static "Avalonia.Media.Brushes" "DeepSkyBlue"))
        (dotnet:invoke overlay "set_BorderThickness" (dotnet:new "Avalonia.Thickness" 4d0))
        (dotnet:invoke overlay "set_IsHitTestVisible" nil)
        (dotnet:invoke overlay "set_IsVisible" nil)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" overlay 0)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumnSpan" overlay 2)
        (dotnet:invoke ebtn "set_Content" "掃き出し (zip)")
        (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" ebtn
                       (dotnet:static "Avalonia.Controls.Dock" "Top"))
        (dotnet:add-event ebtn "Click" (lambda (s e) (declare (ignore s e)) (%pane-export vw)))
        (dotnet:invoke scr "set_Content" stack)
        (let ((pc (dotnet:invoke pane "get_Children")))
          (dotnet:invoke pc "Add" ebtn)
          (dotnet:invoke pc "Add" scr))
        (let ((rc (dotnet:invoke root "get_Children")))
          (dotnet:invoke rc "Add" vport)
          (dotnet:invoke rc "Add" pane)
          (dotnet:invoke rc "Add" overlay))   ; 最後に追加 = 最前面
        (setf (viewer-clip-pane vw) pane (viewer-clip-stack vw) stack
              (viewer-dnd-overlay vw) overlay)
        ;; ペインへの DnD で素材追加 (#18 Phase 2b)。DnD 中はペイン背景を強調 (#20)。
        (dotnet:static "Paalam.DragDropHelper" "EnableClipDrop" pane
                       (dotnet:make-delegate "System.Action`1[System.String]"
                                             (lambda (p) (%clip-drop vw p)))
                       (dotnet:make-delegate "System.Action`1[System.Boolean]"
                                             (lambda (on)
                                               (dotnet:invoke pane "set_Background"
                                                 (dotnet:static "Avalonia.Media.Brushes"
                                                                (if on "SteelBlue" "DimGray"))))))
        ;; メニューバー (#21): Window.Content を DockPanel [Menu(Top) | root] に包む。
        (let ((dock (dotnet:new "Avalonia.Controls.DockPanel"))
              (menu (%build-menu vw win)))
          (setf (viewer-menu vw) menu)
          (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" menu
                         (dotnet:static "Avalonia.Controls.Dock" "Top"))
          (let ((dc (dotnet:invoke dock "get_Children")))
            (dotnet:invoke dc "Add" menu)
            (dotnet:invoke dc "Add" root))
          (dotnet:invoke win "set_Content" dock)))
      (%apply-background vw)          ; 背景色 (= 余白/レターボックス, #2)
      (%apply-interpolation vw)       ; 拡大補間品質 (#2)
      (dotnet:invoke win "set_Width"  1200d0)
      (dotnet:invoke win "set_Height" 800d0)
      (dotnet:add-event win "KeyDown"
                        (lambda (s e) (declare (ignore s)) (%on-key vw e)))
      ;; 起動直後からキー操作が効くよう、表示時にビューポートへフォーカス。
      (dotnet:add-event win "Opened"
                        (lambda (s e) (declare (ignore s e)) (%focus-viewport vw)))
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
                          (ignore-errors (save-history))   ; 最終ページを永続化 (#7)
                          (ignore-errors (loader-stop (viewer-loader vw)))
                          (book-close book)))
      ;; ドラッグ&ドロップで別の本を開く (#19)。配線は C# シムに封じ込め。
      ;; DnD 中はウィンドウ全面にハイライト枠を出す (#20)。
      (dotnet:static "Paalam.DragDropHelper" "Enable" win
                     (dotnet:make-delegate "System.Action`1[System.String]"
                                           (lambda (p) (open-path vw p)))
                     (dotnet:make-delegate "System.Action`1[System.Boolean]"
                                           (lambda (on) (dotnet:invoke (viewer-dnd-overlay vw) "set_IsVisible" (and on t)))))
      ;; 先読みローダーを起動 (vw を closure で参照するので vw 作成後)。
      (setf (viewer-loader vw)
            (open-loader path
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
    ;; 0.1.11 で dotnet:new が全 optional 引数 ctor を補えるので引数なしで OK (#292 解決)。
    (handler-case
        (let ((styles (dotnet:invoke self "get_Styles"))
              (theme (dotnet:new "Avalonia.Themes.Fluent.FluentTheme")))
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
  (setf *debug* (let ((e (dotnet:static "System.Environment" "GetEnvironmentVariable" "PAALAM_DEBUG")))
                  (and e (plusp (length e)) t)))
  (load-settings)
  (load-keybinds)
  (load-history)
  (load-bookmarks)
  (load-clip)
  (setf *target-path* target)
  (let* ((b    (dotnet:static-generic "Avalonia.AppBuilder" "Configure" (list "Paalam.App")))
         (b    (dotnet:static "Avalonia.AppBuilderDesktopExtensions" "UsePlatformDetect" b))
         (args (dotnet:static-generic "System.Array" "Empty" (list "System.String"))))
    (dotnet:static "Avalonia.ClassicDesktopStyleApplicationLifetimeExtensions"
                   "StartWithClassicDesktopLifetime"
                   b args
                   (dotnet:static "Avalonia.Controls.ShutdownMode" "OnLastWindowClose"))))
