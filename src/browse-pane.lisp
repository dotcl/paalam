;;;; browse-pane.lisp — paalam (#39 main.lisp 分割): フォルダツリー本棚 (#6 B)
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; フォルダツリー本棚 (#6 B): サイドにファイルシステムのツリー。ドライブをルートに
;;; 遅延展開 (選択でその直下を読む)。クリック=展開ナビ、ダブルクリック=本として開く
;;; (クリックだけで巨大フォルダを recursive 展開しないように分ける)。
;;; ------------------------------------------------------------------
(defparameter *book-file-extensions*
  '("zip" "cbz" "rar" "cbr" "7z" "cb7" "tar" "cbt" "pdf")
  "ツリーで本として扱うファイル拡張子 (フォルダ + これら)。")
(defun %book-file-p (name)
  (let ((ext (pathname-type name)))
    (and ext (member (string-downcase ext) *book-file-extensions* :test #'string=) t)))

(defun %dir-children (dir)
  "DIR 直下の (label . fullpath): サブフォルダ(自然順) → 本ファイル(自然順)。アクセス不可は無視。"
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
    (dotnet:invoke it "set_Tag" path)   ; Tag=フルパス (文字列なので marshal は素直)
    it))

(defun %browse-populate (item dir)
  "ITEM (フォルダノード) の子を未展開なら DIR 直下で埋める。"
  (let ((items (dotnet:invoke item "get_Items")))
    (when (zerop (dotnet:invoke items "get_Count"))
      (dolist (e (%dir-children dir))
        (dotnet:invoke items "Add" (%browse-tvi (car e) (cdr e)))))))

(defun %rebuild-browse-pane (vw)
  "ルート (論理ドライブ) を並べる。配下は選択時に遅延展開。"
  (let ((items (dotnet:invoke (viewer-browse-tree vw) "get_Items")))
    (dotnet:invoke items "Clear")
    (ignore-errors
      (dolist (d (%enum->list (dotnet:static "System.IO.Directory" "GetLogicalDrives")))
        (dotnet:invoke items "Add" (%browse-tvi d d))))))

(defun %path-ancestors (path)
  "PATH の祖先フルパス列を ルート(ドライブ)→PATH の順で。"
  (let ((acc (list path)) (cur path))
    (loop
      (let ((parent (ignore-errors (dotnet:static "System.IO.Path" "GetDirectoryName" cur))))
        (if (and parent (plusp (length parent)) (not (string-equal parent cur)))
            (progn (push parent acc) (setf cur parent))
            (return))))
    acc))

(defun %browse-find-child (items path)
  "ITEMS (TreeViewItem の Items) から Tag=PATH のノードを探す (大小無視)。"
  (let ((n (dotnet:invoke items "get_Count")))
    (dotimes (i n)
      (let ((it (dotnet:invoke items "get_Item" i)))
        (when (string-equal (or (dotnet:invoke it "get_Tag") "") path)
          (return-from %browse-find-child it))))))

(defun %browse-reveal (vw target)
  "ドライブから TARGET まで辿って各ノードを展開し、最深まで届いたノードを選択+可視化。
TARGET がツリーに出ない種別 (素の画像等) なら、辿れた最後のフォルダを選ぶ。"
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
      ;; 開いている本のディレクトリまで展開・選択する。
      (ignore-errors (%browse-reveal vw (viewer-source vw))))))

