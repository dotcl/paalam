;;;; book.lisp — paalam (#39 main.lisp split): book protocol + each book + open-book
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; book protocol
;;; ------------------------------------------------------------------

(defgeneric book-page-count (book)        (:documentation "Number of pages."))
(defgeneric book-page-name  (book index)  (:documentation "Display name of page index."))
(defgeneric book-load-image (book index)  (:documentation "Avalonia Bitmap of page index."))
(defgeneric book-close      (book)        (:documentation "Release resources."))

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

;;; .NET-based path helpers. CL's pathname / directory functions mishandle Windows UNC
;;; (\\nas\share\...) paths, so anything touching real filesystem paths goes through
;;; System.IO.Path / System.IO.Directory, which handle UNC, mapped drives and unicode.
(defun %path-ext (p)
  "Lowercase extension without the dot of path P (System.IO.Path). \"\" if none."
  (let ((e (dotnet:static "System.IO.Path" "GetExtension" p)))
    (if (and e (plusp (length e)) (char= (char e 0) #\.))
        (string-downcase (subseq e 1))
        "")))
(defun %path-file (p) (dotnet:static "System.IO.Path" "GetFileName" p))
(defun %path-dir  (p) (dotnet:static "System.IO.Path" "GetDirectoryName" p))

(defun %dir-image-files (dir)
  "Full paths of image files directly under DIR (+ subfolders if *recursive*), natural order.
Uses System.IO.Directory.EnumerateFiles so UNC/NAS paths and unicode work reliably
(CL `directory` mishandles UNC). Partial results survive a mid-enumeration access error."
  (let ((opt (dotnet:static "System.IO.SearchOption"
                            (if *recursive* "AllDirectories" "TopDirectoryOnly")))
        (out '()))
    (ignore-errors
      (let* ((files (dotnet:static "System.IO.Directory" "EnumerateFiles" dir "*" opt))
             (en (dotnet:invoke files "GetEnumerator")))
        (loop while (dotnet:invoke en "MoveNext")
              do (let ((p (dotnet:invoke en "get_Current")))
                   (when (image-name-p p) (push p out))))))
    (sort (%dedup-strings out) #'natural<)))

(defun make-directory-book (dir)
  (make-instance 'directory-book :paths (coerce (%dir-image-files dir) 'vector)))

(defmethod book-page-count ((b directory-book)) (length (db-paths b)))
(defmethod book-page-name  ((b directory-book) i) (%path-file (aref (db-paths b) i)))
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

;;; sharp-book (SharpCompress: rar/7z/tar etc.) --------------------
;;; Read archives other than zip via SharpCompress's IArchive. For rar/7z/tar etc.
;;; format detection is done by ArchiveFactory.Open, so a single backend suffices.

(defun %enum->list (enumerable)
  "Convert an IEnumerable into a Lisp list (GetEnumerator/MoveNext/Current)."
  (let ((en (dotnet:invoke enumerable "GetEnumerator")) (acc '()))
    (loop while (dotnet:invoke en "MoveNext")
          do (push (dotnet:invoke en "get_Current") acc))
    (nreverse acc)))

(defun %basename (key)
  "Last component of an archive entry name (/ or \\ separated)."
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
  ;; Open(string, ReaderOptions=null): pass the optional argument explicitly as nil (#292 workaround).
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
                ;; #294 fixed (0.1.12): direct call can select Bitmap(Stream).
                (dotnet:new "Avalonia.Media.Imaging.Bitmap" ms))
      (dotnet:invoke es "Dispose")
      (dotnet:invoke ms "Dispose"))))
(defmethod book-close ((b sharp-book))
  (ignore-errors (dotnet:invoke (sb2-archive b) "Dispose")))

;;; pdf-book (via PDFtoImage/PDFium) --------------------------------
;;; Rasterize each page with the C# shim Paalam.PdfDoc into an Avalonia Bitmap.

(defparameter *pdf-dpi* 300
  "Rasterization resolution of PDF pages (DPI). A fixed 150 looks coarse when scaled up to 4K, so make it variable (#2).
Higher is more detailed but uses more memory/time. For 4K, around 300-400 is a rough guide. After changing, reopen the book to redraw.")

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

;;; book-page-bytes: return the raw bytes and extension of a page for clip export (#18).
;;; (values byte[] extension). dir/zip/sharp keep the stored format as-is; pdf is converted to PNG.
(defgeneric book-page-bytes (book index))

(defun %name-ext (name) (string-downcase (or (pathname-type name) "dat")))

(defmethod book-page-bytes ((b directory-book) i)
  (let ((path (aref (db-paths b) i)))
    (values (dotnet:static "System.IO.File" "ReadAllBytes" path) (%path-ext path))))

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

;;; book-page-file-info: source size / mtime / full path of a page for the info pane (#36).
;;; (values size-bytes mtime-string full-path). Each piece is nil when not applicable
;;; (pdf has no per-page file; archives have no on-disk path). Best-effort; never throws.

(defun %dt->string (dt)
  "Format a .NET DateTime / DateTimeOffset as 'yyyy-MM-dd HH:mm:ss' (nil on failure)."
  (ignore-errors (dotnet:invoke dt "ToString" "yyyy-MM-dd HH:mm:ss")))

(defgeneric book-page-file-info (book index)
  (:documentation "(values size-bytes mtime-string full-path) for the page source; nil parts where N/A."))

(defmethod book-page-file-info ((b t) i) (declare (ignore i)) (values nil nil nil))

(defmethod book-page-file-info ((b directory-book) i)
  (or (ignore-errors
        (let* ((path (aref (db-paths b) i))
               (fi (dotnet:new "System.IO.FileInfo" path)))
          (values (dotnet:invoke fi "get_Length")
                  (%dt->string (dotnet:invoke fi "get_LastWriteTime"))
                  path)))
      (values nil nil nil)))

(defmethod book-page-file-info ((b zip-book) i)
  (or (ignore-errors
        (let ((e (aref (zb-entries b) i)))
          (values (dotnet:invoke e "get_Length")              ; uncompressed size
                  (%dt->string (dotnet:invoke e "get_LastWriteTime"))   ; DateTimeOffset
                  nil)))
      (values nil nil nil)))

(defmethod book-page-file-info ((b sharp-book) i)
  ;; LastModifiedTime is Nullable<DateTime> (awkward via FFI), so size only.
  (or (ignore-errors (values (dotnet:invoke (aref (sb2-entries b) i) "get_Size") nil nil))
      (values nil nil nil)))

;;; book-doc-info: document-level metadata for the info pane (#36). alist of (label . value),
;;; nil when none. Only PDF has it for now (title/author/producer via pdfium FPDF_GetMetaText).
(defgeneric book-doc-info (book)
  (:documentation "alist (label . value) of document-level metadata; nil if none."))

(defmethod book-doc-info ((b t)) nil)

(defmethod book-doc-info ((b pdf-book))
  (flet ((m (tag)
           (let ((s (ignore-errors (dotnet:invoke (pdfb-doc b) "Meta" tag))))
             (and (stringp s) (plusp (length s)) s))))
    (loop for (label . tag) in '(("Title" . "Title") ("Author" . "Author")
                                 ("Subject" . "Subject") ("Producer" . "Producer"))
          for v = (m tag)
          when v collect (cons label v))))

;;; open-book --------------------------------------------------------

(defun open-book (path)
  "Open PATH (directory / archive / image file) as a book.
Returns two values (book start-index)."
  (cond
    ;; Unspecified (path = nil): open an empty book (window with no book opened).
    ((null path)
     (values (make-instance 'directory-book :paths #()) 0))
    ((dotnet:static "System.IO.Directory" "Exists" path)
     (values (make-directory-book path) 0))
    ((dotnet:static "System.IO.File" "Exists" path)
     (let ((ext (%path-ext path)))   ; .NET-based: UNC/NAS-safe
       (cond
         ((member ext '("zip" "cbz") :test #'string=)
          (values (make-zip-book path) 0))
         ((member ext '("rar" "cbr" "7z" "cb7" "tar" "cbt") :test #'string=)
          (values (make-sharp-book path) 0))
         ((string= ext "pdf")
          (values (make-pdf-book path) 0))
         ((image-ext-p ext)
          (let* ((dir  (%path-dir path))
                 (book (make-directory-book dir))
                 (start (or (position (%path-file path) (db-paths book)
                                      :key #'%path-file :test #'string-equal)
                            0)))
            (values book start)))
         (t (error "Unsupported file format: ~a" path)))))
    (t (error "Path not found: ~a" path))))

