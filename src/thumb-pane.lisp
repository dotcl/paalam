;;;; thumb-pane.lisp — paalam (#39 main.lisp 分割): ページサムネ本棚 (#6 A)
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; ページサムネ本棚 (#6 A): サイドペインに現在の本の全ページを縮小サムネで並べ、
;;; クリックでそのページへジャンプ。サムネは DecodeToWidth で縮小デコード (全ページ分
;;; フル Bitmap を抱えないため)。背景スレッドで非同期ロードして後から差し込む。
;;; ------------------------------------------------------------------
(defparameter *thumb-width* 150 "ページサムネの幅 (px)。")
(defparameter *page-thumb-cache* (make-hash-table :test 'equal)
  "(source . page) -> 縮小サムネ Bitmap。本を変えたらクリア。")
(defun %pthumb-get (src page) (gethash (cons src page) *page-thumb-cache*))
(defun %pthumb-put (src page bmp) (setf (gethash (cons src page) *page-thumb-cache*) bmp))
(defun %pthumb-clear ()
  (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (dotnet:invoke v "Dispose")))
           *page-thumb-cache*)
  (clrhash *page-thumb-cache*))

(defun %make-page-thumb (book index width)
  "BOOK の INDEX ページを WIDTH px に縮小デコードした Bitmap。バイト列から DecodeToWidth。
MemoryStream(byte[]) ctor は overload バグ (#294) を踏むので無引数 + Write で渡す。"
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
  "PAGE を含む見開きへ移動 (本棚クリック)。フォーカスはビューポートへ戻す。"
  (setf (viewer-sindex vw) (spread-index-of (viewer-spreads vw) page))
  (show-spread vw)
  (%focus-viewport vw))

(defun %rebuild-thumb-pane (vw)
  "thumb-stack を現在の本の全ページサムネで作り直す。キャッシュ済みは即表示、
未キャッシュは背景スレッドで縮小デコードして差し込む (本は別ハンドルで開き直す)。"
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
            (dotnet:invoke e "set_Handled" t)   ; ウィンドウのパンを起こさない
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
  "本を変えたとき: サムネ表示を作り直し、旧サムネ Bitmap は描画後に破棄 (use-after-dispose 回避)。"
  (let ((old '()))
    (maphash (lambda (k v) (declare (ignore k)) (push v old)) *page-thumb-cache*)
    (clrhash *page-thumb-cache*)   ; 新しい本は別キーなので参照だけ落とす
    (when (and (viewer-thumb-pane vw)
               (dotnet:invoke (viewer-thumb-pane vw) "get_IsVisible"))
      (%rebuild-thumb-pane vw))    ; 旧 Image を tstack から外す (Clear)
    (when old
      (%defer (lambda () (dolist (b old) (ignore-errors (dotnet:invoke b "Dispose"))))))))

