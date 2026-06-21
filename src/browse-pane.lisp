;;;; browse-pane.lisp — paalam (#39 main.lisp split): folder-tree shelf (#6 B)
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; Folder-tree shelf (#6 B): a filesystem tree on the side. Drives as roots.
;;; Lazy expand (select reads the immediate children). Click=expand/navigate, double-click=open as book
;;; (split so that a single click does not recursively expand a huge folder).
;;; ------------------------------------------------------------------
(defparameter *book-file-extensions*
  '("zip" "cbz" "rar" "cbr" "7z" "cb7" "tar" "cbt" "pdf")
  "File extensions treated as books in the tree (folders + these).")
(defun %book-file-p (name)
  (let ((ext (pathname-type name)))
    (and ext (member (string-downcase ext) *book-file-extensions* :test #'string=) t)))

(defun %dir-children (dir)
  "Immediate children of DIR as (label . fullpath): subfolders (natural order) -> book files (natural order). Inaccessible entries are ignored."
  (let ((folders '()) (files '()))
    (ignore-errors
      (dolist (d (%enum->list (dotnet:static "System.IO.Directory" "GetDirectories" dir)))
        (push d folders)))
    (ignore-errors
      (dolist (f (%enum->list (dotnet:static "System.IO.Directory" "GetFiles" dir)))
        (when (%book-file-p f) (push f files))))
    (append (mapcar (lambda (p) (cons (%basename p) p)) (sort folders #'natural<))
            (mapcar (lambda (p) (cons (%basename p) p)) (sort files #'natural<)))))

(defun %browse-tvi (label path)
  (let ((it (dotnet:new "Avalonia.Controls.TreeViewItem")))
    (dotnet:invoke it "set_Header" label)
    (dotnet:invoke it "set_Tag" path)   ; Tag=full path (a string, so marshalling is straightforward)
    it))

(defun %browse-populate (item dir)
  "If ITEM (a folder node) is not yet expanded, fill its children with the immediate entries of DIR."
  (let ((items (dotnet:invoke item "get_Items")))
    (when (zerop (dotnet:invoke items "get_Count"))
      (dolist (e (%dir-children dir))
        (dotnet:invoke items "Add" (%browse-tvi (car e) (cdr e)))))))

(defun %rebuild-browse-pane (vw)
  "List the roots (logical drives). Children are lazily expanded on selection."
  (let ((items (dotnet:invoke (viewer-browse-tree vw) "get_Items")))
    (dotnet:invoke items "Clear")
    (ignore-errors
      (dolist (d (%enum->list (dotnet:static "System.IO.Directory" "GetLogicalDrives")))
        (dotnet:invoke items "Add" (%browse-tvi d d))))))

(defun %path-ancestors (path)
  "The sequence of ancestor full paths of PATH, ordered root (drive) -> PATH."
  (let ((acc (list path)) (cur path))
    (loop
      (let ((parent (ignore-errors (dotnet:static "System.IO.Path" "GetDirectoryName" cur))))
        (if (and parent (plusp (length parent)) (not (string-equal parent cur)))
            (progn (push parent acc) (setf cur parent))
            (return))))
    acc))

(defun %browse-find-child (items path)
  "Find the node with Tag=PATH within ITEMS (a TreeViewItem's Items) (case-insensitive)."
  (let ((n (dotnet:invoke items "get_Count")))
    (dotimes (i n)
      (let ((it (dotnet:invoke items "get_Item" i)))
        (when (string-equal (or (dotnet:invoke it "get_Tag") "") path)
          (return-from %browse-find-child it))))))

(defun %browse-reveal (vw target)
  "Walk from the drive down to TARGET, expanding each node, then select and reveal the deepest node reached.
If TARGET is a kind that does not appear in the tree (e.g. a bare image), select the last folder reached."
  (when (and target (plusp (length target)))
    (let ((items (dotnet:invoke (viewer-browse-tree vw) "get_Items")) (node nil))
      (dolist (a (%path-ancestors target))
        (let ((child (%browse-find-child items a)))
          (unless child (return))
          (setf node child)
          (when (dotnet:static "System.IO.Directory" "Exists" a)
            (%browse-populate node a)
            (dotnet:invoke node "set_IsExpanded" t))
          (setf items (dotnet:invoke node "get_Items"))))
      (when node
        (dotnet:invoke node "set_IsSelected" t)
        (ignore-errors (dotnet:invoke node "BringIntoView"))))))

(defun %toggle-browse-pane (vw)
  (let* ((pane (viewer-browse-pane vw))
         (vis (dotnet:invoke pane "get_IsVisible")))
    (dotnet:invoke pane "set_IsVisible" (not vis))
    (unless vis
      (when (zerop (dotnet:invoke (dotnet:invoke (viewer-browse-tree vw) "get_Items") "get_Count"))
        (%rebuild-browse-pane vw))
      ;; Expand and select down to the directory of the currently open book.
      (ignore-errors (%browse-reveal vw (viewer-source vw))))))

