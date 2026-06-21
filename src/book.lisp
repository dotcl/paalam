;;;; book.lisp — paalam (#39 main.lisp 分割): book プロトコル + 各 book + open-book
(in-package :paalam)

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
                ;; #294 修正済 (0.1.12): 直呼びで Bitmap(Stream) を選べる。
                (dotnet:new "Avalonia.Media.Imaging.Bitmap" ms))
      (dotnet:invoke es "Dispose")
      (dotnet:invoke ms "Dispose"))))
(defmethod book-close ((b sharp-book))
  (ignore-errors (dotnet:invoke (sb2-archive b) "Dispose")))

;;; pdf-book (PDFtoImage/PDFium 経由) -------------------------------
;;; 各ページを C# シム Paalam.PdfDoc でラスタライズして Avalonia Bitmap に。

(defparameter *pdf-dpi* 300
  "PDF ページのラスタライズ解像度 (DPI)。150 固定だと 4K で拡大され粗いので可変に (#2)。
高いほど精細だがメモリ/時間増。4K は 300〜400 程度が目安。変更時は本を開き直して再描画。")

(defclass pdf-book ()
  ((doc   :initarg :doc   :reader pdfb-doc)
   (count :initarg :count :reader pdfb-count)))

(defun make-pdf-book (path)
  (let* ((doc (dotnet:new "Paalam.PdfDoc" path))
         (n   (dotnet:invoke doc "get_PageCount")))
    (make-instance 'pdf-book :doc doc :count n)))

(defmethod book-page-count ((b pdf-book)) (pdfb-count b))
(defmethod book-page-name  ((b pdf-book) i) (format nil "p~d" (1+ i)))
(defmethod book-load-image ((b pdf-book) i) (dotnet:invoke (pdfb-doc b) "RenderPage" i *pdf-dpi*))
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
  (values (dotnet:invoke (pdfb-doc b) "RenderPagePng" i *pdf-dpi*) "png"))

;;; open-book --------------------------------------------------------

(defun open-book (path)
  "PATH (ディレクトリ / アーカイブ / 画像ファイル) を本として開く。
2 値 (book start-index) を返す。"
  (cond
    ;; 未指定 (path = nil): 空の本を開く (本未オープン状態の窓)。
    ((null path)
     (values (make-instance 'directory-book :paths #()) 0))
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

