;;;; thumb-pane.lisp — paalam (#39 main.lisp split): page-thumbnail shelf (#6 A)
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; Page-thumbnail shelf (#6 A): lay out all pages of the current book as
;;; downscaled thumbnails in a side pane, and click to jump to that page. Each
;;; thumbnail is downscale-decoded via DecodeToWidth (so we don't hold a full
;;; Bitmap for every page). Loaded asynchronously on a background thread and
;;; spliced in afterward.
;;; ------------------------------------------------------------------
(defparameter *thumb-width* 150 "Width of a page thumbnail (px).")
(defparameter *page-thumb-cache* (make-hash-table :test 'equal)
  "(source . page) -> downscaled thumbnail Bitmap. Cleared when the book changes.")
(defun %pthumb-get (src page) (gethash (cons src page) *page-thumb-cache*))
(defun %pthumb-put (src page bmp) (setf (gethash (cons src page) *page-thumb-cache*) bmp))
(defun %pthumb-clear ()
  (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (dotnet:invoke v "Dispose")))
           *page-thumb-cache*)
  (clrhash *page-thumb-cache*))

(defun %make-page-thumb (book index width)
  "Bitmap of BOOK's INDEX page downscale-decoded to WIDTH px. DecodeToWidth from a byte array.
The MemoryStream(byte[]) ctor hits an overload bug (#294), so pass via no-arg ctor + Write."
  (multiple-value-bind (bytes ext) (book-page-bytes book index)
    (declare (ignore ext))
    (let ((ms (dotnet:new "System.IO.MemoryStream")))
      (unwind-protect
           (progn
             (dotnet:invoke ms "Write" bytes 0 (dotnet:invoke bytes "get_Length"))
             (dotnet:invoke ms "set_Position" 0)
             (dotnet:static "Avalonia.Media.Imaging.Bitmap" "DecodeToWidth" ms width))
        (dotnet:invoke ms "Dispose")))))

(defun %jump-to-page (vw page)
  "Move to the spread containing PAGE (shelf click). Return focus to the viewport."
  (setf (viewer-sindex vw) (spread-index-of (viewer-spreads vw) page))
  (show-spread vw)
  (%focus-viewport vw))

(defun %rebuild-thumb-pane (vw)
  "Rebuild thumb-stack with thumbnails for every page of the current book. Cached
ones display immediately; uncached ones are downscale-decoded on a background thread
and spliced in (the book is reopened with a separate handle)."
  (let* ((stack (viewer-thumb-stack vw))
         (children (dotnet:invoke stack "get_Children"))
         (src (viewer-source vw))
         (n (book-page-count (viewer-book vw)))
         (pending '()))
    (dotnet:invoke children "Clear")
    (dotimes (i n)
      (let ((img (dotnet:new "Avalonia.Controls.Image"))
            (page i)
            (cached (%pthumb-get src i)))
        (dotnet:invoke img "set_Width" (coerce *thumb-width* 'double-float))
        (dotnet:invoke img "set_Stretch" (dotnet:static "Avalonia.Media.Stretch" "Uniform"))
        (dotnet:invoke img "set_Margin" (dotnet:new "Avalonia.Thickness" 4d0 3d0 4d0 3d0))
        (dotnet:add-event img "PointerPressed"
          (lambda (s e) (declare (ignore s))
            (dotnet:invoke e "set_Handled" t)   ; don't trigger window panning
            (%jump-to-page vw page)))
        (if cached
            (dotnet:invoke img "set_Source" cached)
            (push (cons img i) pending))
        (dotnet:invoke children "Add" img)))
    (when pending
      (setf pending (nreverse pending))
      (make-thread
       (lambda ()
         (let ((book (ignore-errors (open-book src))))
           (when book
             (unwind-protect
                  (dolist (pair pending)
                    (ignore-errors
                      (let* ((img (car pair)) (page (cdr pair))
                             (bmp (%make-page-thumb book page *thumb-width*)))
                        (when bmp
                          (%pthumb-put src page bmp)
                          (%ui-post (lambda () (dotnet:invoke img "set_Source" bmp)))))))
               (ignore-errors (book-close book))))))
       :name "paalam-page-thumbs"))))

(defun %toggle-thumb-pane (vw)
  (let* ((pane (viewer-thumb-pane vw))
         (vis (dotnet:invoke pane "get_IsVisible")))
    (dotnet:invoke pane "set_IsVisible" (not vis))
    (unless vis (%rebuild-thumb-pane vw))))

(defun %thumb-pane-refresh (vw)
  "When the book changes: rebuild the thumbnail display and dispose the old thumbnail Bitmaps after rendering (avoid use-after-dispose)."
  (let ((old '()))
    (maphash (lambda (k v) (declare (ignore k)) (push v old)) *page-thumb-cache*)
    (clrhash *page-thumb-cache*)   ; the new book uses different keys, so just drop the references
    (when (and (viewer-thumb-pane vw)
               (dotnet:invoke (viewer-thumb-pane vw) "get_IsVisible"))
      (%rebuild-thumb-pane vw))    ; remove old Images from tstack (Clear)
    (when old
      (%defer (lambda () (dolist (b old) (ignore-errors (dotnet:invoke b "Dispose"))))))))

