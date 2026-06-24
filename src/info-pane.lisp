;;;; info-pane.lisp — paalam (#39 split): file info / EXIF panel (#36)
(in-package :paalam)

;;; A NeeView-style "file information" side pane. Shows metadata of the current
;;; page in a grouped list. Phase 1 = basic info with no extra dependencies
;;; (name/path/page/format/size/mtime/dimensions/book type). EXIF (Phase 2) will
;;; add more groups into the same pane via MetadataExtractor.

(defparameter *info-pane-width* 260 "Width of the info pane (px).")

(defun %human-size (bytes)
  "Human-readable byte count. nil -> em dash."
  (cond ((null bytes) "—")
        ((< bytes 1024) (format nil "~d B" bytes))
        ((< bytes (* 1024 1024)) (format nil "~,1f KB" (/ bytes 1024d0)))
        ((< bytes (* 1024 1024 1024)) (format nil "~,1f MB" (/ bytes (* 1024d0 1024))))
        (t (format nil "~,2f GB" (/ bytes (* 1024d0 1024 1024))))))

(defun %book-type-name (book)
  (typecase book
    (pdf-book "PDF")
    (zip-book "ZIP")
    (sharp-book "Archive")
    (clip-book "Clip")
    (directory-book "Folder")
    (t "—")))

(defun %tsv-lines (s)
  "Parse \"label<TAB>value\" newline-separated lines into a list of (label . value)."
  (let ((out '()) (i 0) (n (length s)))
    (loop while (< i n)
          for nl = (or (position #\Newline s :start i) n)
          do (let* ((line (subseq s i nl))
                    (tab (position #\Tab line)))
               (when tab (push (cons (subseq line 0 tab) (subseq line (1+ tab))) out)))
             (setf i (1+ nl)))
    (nreverse out)))

(defun %page-exif (book index)
  "EXIF/metadata of page INDEX as a list of (label . value). nil if none/unreadable (#36 Phase 2).
Reads the page's raw bytes (originals for dir/zip/sharp/clip; PDF renders carry none)."
  (ignore-errors
    (multiple-value-bind (bytes ext) (book-page-bytes book index)
      (declare (ignore ext))
      (let ((s (dotnet:static "Paalam.Exif" "Summary" bytes)))
        (and (stringp s) (plusp (length s)) (%tsv-lines s))))))

(defun %page-dimensions (vw page)
  "\"W x H\" of PAGE from the loader-cached bitmap, or nil if not decoded yet."
  (let ((bmp (%page-bitmap vw page)))
    (when bmp
      (ignore-errors
        (let ((ps (dotnet:invoke bmp "get_PixelSize")))
          (format nil "~d x ~d"
                  (dotnet:invoke ps "get_Width")
                  (dotnet:invoke ps "get_Height")))))))

(defun %info-header (text)
  (let ((tb (dotnet:new "Avalonia.Controls.TextBlock")))
    (dotnet:invoke tb "set_Text" text)
    (dotnet:invoke tb "set_FontWeight" (dotnet:static "Avalonia.Media.FontWeight" "Bold"))
    (dotnet:invoke tb "set_Foreground" (dotnet:static "Avalonia.Media.Brushes" "White"))
    (dotnet:invoke tb "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 8d0 0d0 2d0))
    tb))

(defun %info-row (vw label value &key on-click on-double)
  "A small gray LABEL above the (wrapping, white) VALUE. nil/blank value -> em dash.
Single-click runs ON-CLICK if given, else copies VALUE to the clipboard (#36 Phase 3).
Double-click runs ON-DOUBLE if given (e.g. rename)."
  (let* ((panel (dotnet:new "Avalonia.Controls.StackPanel"))
         (l (dotnet:new "Avalonia.Controls.TextBlock"))
         (v (dotnet:new "Avalonia.Controls.TextBlock"))
         (copyable (and (stringp value) (plusp (length value)))))
    (dotnet:invoke panel "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 1d0 0d0 3d0))
    (dotnet:invoke l "set_Text" label)
    (dotnet:invoke l "set_FontSize" 11d0)
    (dotnet:invoke l "set_Foreground" (dotnet:static "Avalonia.Media.Brushes" "Silver"))
    (dotnet:invoke v "set_Text" (if copyable value "—"))
    (dotnet:invoke v "set_Foreground" (dotnet:static "Avalonia.Media.Brushes" "White"))
    (dotnet:invoke v "set_TextWrapping" (dotnet:static "Avalonia.Media.TextWrapping" "Wrap"))
    (when (or on-click on-double copyable)
      (dotnet:add-event panel "PointerPressed"
        (lambda (s e) (declare (ignore s))
          (dotnet:invoke e "set_Handled" t)
          (cond (on-click (funcall on-click))
                (copyable (%copy-text vw value))))))
    (when on-double
      (dotnet:add-event panel "DoubleTapped"
        (lambda (s e) (declare (ignore s e)) (funcall on-double))))
    (let ((c (dotnet:invoke panel "get_Children")))
      (dotnet:invoke c "Add" l)
      (dotnet:invoke c "Add" v))
    panel))

(defun %copy-text (vw text)
  "Copy TEXT to the window clipboard with a brief status note (#36 Phase 3)."
  (ignore-errors
    (let ((clip (dotnet:invoke (viewer-window vw) "get_Clipboard")))
      (when clip (dotnet:invoke clip "SetTextAsync" text))))
  (ignore-errors (%status vw (format nil "Copied: ~a" text))))

(defun %open-map (coords)
  "Open \"lat, lon\" COORDS in the default map service (#36 Phase 3)."
  (%open-url (format nil "https://www.google.com/maps?q=~a" (remove #\Space coords))))

(defun %rename-current-file (vw)
  "Rename the selected page's file on disk (directory-book only; archives unsupported) (#36 Phase 3).
Prompts for the new name, File.Move, then reopens to resync book/loader/thumbnails."
  (let ((book (viewer-book vw)) (sel (viewer-info-page vw)))
    (when (and (typep book 'directory-book) sel)
      (let ((old (aref (db-paths book) sel)))
        (%prompt-text (viewer-window vw) "Rename file" (%path-file old)
          (lambda (newname)
            (let ((new (dotnet:static "System.IO.Path" "Combine" (%path-dir old) newname)))
              (cond
                ((string= new old))
                ((%file-exists new)
                 (%status vw "Rename: a file with that name already exists"))
                (t (handler-case
                       (progn
                         (dotnet:static "System.IO.File" "Move" old new)
                         (when (string= (viewer-source vw) old) (setf (viewer-source vw) new))
                         (%reopen vw)
                         (%info-pane-refresh-if-visible vw)
                         (%status vw (format nil "Renamed to ~a" newname)))
                     (error (e) (log-err "rename" e)
                       (%status vw "Rename failed (see log)"))))))))))))

(defun %info-spread-pages (vw)
  "Pages of the current spread in screen order (left, right), honoring binding direction (#36)."
  (let ((sp (viewer-spreads vw)))
    (when (plusp (length sp))
      (let ((pages (aref sp (viewer-sindex vw))))
        (if (= (length pages) 2)
            (destructuring-bind (lo hi) pages
              (if (eq *page-direction* :rtl) (list hi lo) (list lo hi)))
            (copy-list pages))))))

(defun %info-thumb (vw page selected)
  "Clickable thumbnail (loader-cached bitmap) for PAGE; SELECTED draws a highlight border.
Clicking selects PAGE so the detail below switches (NeeView-style left/right pick in spread; #36)."
  (let ((border (dotnet:new "Avalonia.Controls.Border"))
        (img (dotnet:new "Avalonia.Controls.Image"))
        (bmp (%page-bitmap vw page)))
    (dotnet:invoke border "set_BorderThickness" (dotnet:new "Avalonia.Thickness" 2d0))
    (dotnet:invoke border "set_BorderBrush"
                   (dotnet:static "Avalonia.Media.Brushes" (if selected "DeepSkyBlue" "Gray")))
    (dotnet:invoke border "set_Margin" (dotnet:new "Avalonia.Thickness" 2d0))
    (dotnet:invoke img "set_Width" 104d0)
    (dotnet:invoke img "set_Stretch" (dotnet:static "Avalonia.Media.Stretch" "Uniform"))
    (when bmp (dotnet:invoke img "set_Source" bmp))
    (dotnet:invoke border "set_Child" img)
    (dotnet:add-event border "PointerPressed"
      (lambda (s e) (declare (ignore s))
        (dotnet:invoke e "set_Handled" t)
        (setf (viewer-info-page vw) page)
        (%rebuild-info-pane vw)))
    border))

(defun %rebuild-info-pane (vw)
  "Populate the info pane with the selected page's metadata (#36).
In a 2-page spread, both page thumbnails are shown in screen order and clicking
left/right chooses which page's details to display."
  (let* ((stack (viewer-info-stack vw))
         (children (dotnet:invoke stack "get_Children"))
         (book (viewer-book vw))
         (n (book-page-count book)))
    (dotnet:invoke children "Clear")
    (if (zerop n)
        (dotnet:invoke children "Add" (%info-row vw "Status" "No book opened"))
        (let* ((pages (%info-spread-pages vw))
               ;; keep the prior left/right pick if still in this spread, else default to the primary page.
               (sel (if (member (viewer-info-page vw) pages) (viewer-info-page vw) (%cur-page vw))))
          (setf (viewer-info-page vw) sel)
          ;; thumbnail row = the spread's pages in screen order; the selected one is highlighted.
          (let ((row (dotnet:new "Avalonia.Controls.StackPanel")))
            (dotnet:invoke row "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
            (dotnet:invoke row "set_HorizontalAlignment" (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Center"))
            (let ((rc (dotnet:invoke row "get_Children")))
              (dolist (p pages) (dotnet:invoke rc "Add" (%info-thumb vw p (eql p sel)))))
            (dotnet:invoke children "Add" row))
          (multiple-value-bind (size mtime path) (book-page-file-info book sel)
            (let* ((name (book-page-name book sel))
                   (ext  (let ((e (pathname-type name))) (and e (string-upcase e))))
                   (dims (%page-dimensions vw sel))
                   (add  (lambda (item) (dotnet:invoke children "Add" item))))
              (funcall add (%info-header "File"))
              ;; Name: click=copy, double-click=rename (directory-book only; #36 Phase 3)
              (funcall add (%info-row vw "Name" name
                                      :on-double (when (typep book 'directory-book)
                                                   (lambda () (%rename-current-file vw)))))
              (funcall add (%info-row vw "Page" (format nil "~d / ~d" (1+ sel) n)))
              (funcall add (%info-row vw "Format" (or ext (%book-type-name book))))
              (funcall add (%info-row vw "Source" (viewer-source vw)))
              (when path (funcall add (%info-row vw "Path" path)))
              (funcall add (%info-row vw "Size" (%human-size size)))
              (when mtime (funcall add (%info-row vw "Modified" mtime)))
              (funcall add (%info-header "Image"))
              (funcall add (%info-row vw "Dimensions" (or dims "loading…")))
              (funcall add (%info-row vw "Type" (%book-type-name book)))
              ;; image EXIF / camera metadata when present (#36 Phase 2)
              (let ((exif (%page-exif book sel)))
                (when exif
                  (funcall add (%info-header "Photo"))
                  (dolist (kv exif)
                    ;; GPS row: click opens the location in a map (#36 Phase 3)
                    (if (string= (car kv) "GPS")
                        (funcall add (%info-row vw "GPS" (cdr kv)
                                                :on-click (let ((c (cdr kv))) (lambda () (%open-map c)))))
                        (funcall add (%info-row vw (car kv) (cdr kv)))))))
              ;; document-level metadata (PDF title/author/...) when available (#36)
              (let ((doc (book-doc-info book)))
                (when doc
                  (funcall add (%info-header "Document"))
                  (dolist (kv doc) (funcall add (%info-row vw (car kv) (cdr kv))))))))))))

(defun %info-pane-refresh-if-visible (vw)
  (when (and (viewer-info-pane vw) (dotnet:invoke (viewer-info-pane vw) "get_IsVisible"))
    (%rebuild-info-pane vw)))

(defun %toggle-info-pane (vw)
  (let* ((pane (viewer-info-pane vw))
         (vis (dotnet:invoke pane "get_IsVisible")))
    (dotnet:invoke pane "set_IsVisible" (not vis))
    (unless vis (%rebuild-info-pane vw))))
