;;;; editor.lisp — paalam (#39 main.lisp 分割): ジェスチャ編集 (#29) + メニュー編集 (#26)
(in-package :paalam)

(defun %gesture-label (id)
  (let ((c (%command id))) (if c (command-label c) (format nil "~a" id))))

(defun %open-gesture-editor (vw)
  "マウスジェスチャ割当の編集ウィンドウ (#29)。記録ボックスでドラッグして方向列を取り、
コマンドを選んで「追加/更新」。既存はリストから削除、既定に戻すも可。"
  (let* ((owner (viewer-window vw))
         (w    (dotnet:new "Avalonia.Controls.Window"))
         (cap  (dotnet:new "Avalonia.Controls.Border"))
         (cap-text  (dotnet:new "Avalonia.Controls.TextBlock"))
         (lst  (dotnet:new "Avalonia.Controls.StackPanel"))
         (choices (mapcar (lambda (c) (cons (command-label c) (command-id c))) *commands*))
         (combo (%s-choice-combo choices (command-id (first *commands*))))
         (capbox (list "" 0d0 0d0 nil)))   ; (seq lx ly active)
    (labels
        ((cap-show ()
           (dotnet:invoke cap-text "set_Text"
                          (let ((s (first capbox)))
                            (if (plusp (length s)) (format nil "記録: ~a" s)
                                "ここでドラッグして方向を記録 (U↑ D↓ L← R→)"))))
         (refresh ()
           (let ((items (dotnet:invoke lst "get_Children")))
             (dotnet:invoke items "Clear")
             (dolist (g *gestures*)
               (let* ((seq (car g)) (id (cdr g))
                      (row (dotnet:new "Avalonia.Controls.DockPanel"))
                      (lbl (dotnet:new "Avalonia.Controls.TextBlock"))
                      (del (%s-button "削除" (let ((s seq)) (lambda () (del-gesture s))))))
                 (dotnet:invoke lbl "set_Text" (format nil "~a  →  ~a" seq (%gesture-label id)))
                 (dotnet:invoke lbl "set_VerticalAlignment"
                                (dotnet:static "Avalonia.Layout.VerticalAlignment" "Center"))
                 (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" del
                                (dotnet:static "Avalonia.Controls.Dock" "Right"))
                 (let ((c (dotnet:invoke row "get_Children")))
                   (dotnet:invoke c "Add" del) (dotnet:invoke c "Add" lbl))
                 (dotnet:invoke row "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 2d0 0d0 2d0))
                 (dotnet:invoke items "Add" row)))))
         (del-gesture (seq)
           (setf *gestures* (remove seq *gestures* :key #'car :test #'string-equal))
           (save-gestures) (refresh))
         (add-gesture ()
           (let ((seq (first capbox)) (id (%s-combo-value combo choices)))
             (when (and (plusp (length seq)) id)
               ;; 同じ方向列の既存割当は置き換え (先頭に積む)。
               (setf *gestures* (cons (cons seq id)
                                      (remove seq *gestures* :key #'car :test #'string-equal)))
               (save-gestures) (refresh))))
         (reset-default ()
           (setf *gestures* (copy-tree *default-gestures*)) (save-gestures) (refresh)))
      (dotnet:invoke cap "set_Height" 90d0)
      (dotnet:invoke cap "set_Background" (dotnet:static "Avalonia.Media.Brushes" "DimGray"))
      (dotnet:invoke cap-text "set_HorizontalAlignment"
                     (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Center"))
      (dotnet:invoke cap-text "set_VerticalAlignment"
                     (dotnet:static "Avalonia.Layout.VerticalAlignment" "Center"))
      (dotnet:invoke cap "set_Child" cap-text)
      (cap-show)
      (dotnet:add-event cap "PointerPressed"
                        (lambda (s e) (declare (ignore s))
                          (multiple-value-bind (x y) (%pos-xy e cap)
                            (setf (first capbox) "" (second capbox) x (third capbox) y
                                  (fourth capbox) t)
                            (cap-show))))
      (dotnet:add-event cap "PointerMoved"
                        (lambda (s e) (declare (ignore s))
                          (when (fourth capbox)
                            (multiple-value-bind (x y) (%pos-xy e cap)
                              (multiple-value-bind (seq lx ly moved)
                                  (%gesture-step (first capbox) (second capbox) (third capbox) x y)
                                (when moved
                                  (setf (first capbox) seq (second capbox) lx (third capbox) ly)
                                  (cap-show)))))))
      (dotnet:add-event cap "PointerReleased"
                        (lambda (s e) (declare (ignore s e))
                          (setf (fourth capbox) nil) (cap-show)))
      (refresh)
      (let* ((root (dotnet:new "Avalonia.Controls.StackPanel"))
             (addrow (dotnet:new "Avalonia.Controls.StackPanel"))
             (btns (dotnet:new "Avalonia.Controls.StackPanel")))
        (dotnet:invoke addrow "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
        (dotnet:invoke addrow "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 8d0 0d0 0d0))
        (let ((c (dotnet:invoke addrow "get_Children")))
          (dotnet:invoke c "Add" combo)
          (dotnet:invoke c "Add" (%s-button "追加/更新" (lambda () (add-gesture)))))
        (dotnet:invoke btns "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
        (dotnet:invoke btns "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 12d0 0d0 0d0))
        (let ((c (dotnet:invoke btns "get_Children")))
          (dotnet:invoke c "Add" (%s-button "既定に戻す" (lambda () (reset-default))))
          (dotnet:invoke c "Add" (%s-button "閉じる" (lambda () (dotnet:invoke w "Close")))))
        (dotnet:invoke root "set_Margin" (dotnet:new "Avalonia.Thickness" 12d0))
        (let ((c (dotnet:invoke root "get_Children")))
          (dotnet:invoke c "Add" (%s-header "マウスジェスチャ (右ドラッグの方向列 → コマンド)"))
          (dotnet:invoke c "Add" cap)
          (dotnet:invoke c "Add" addrow)
          (dotnet:invoke c "Add" (%s-header "現在の割当"))
          (dotnet:invoke c "Add" lst)
          (dotnet:invoke c "Add" btns))
        (dotnet:invoke w "set_Content" root))
      (dotnet:invoke w "set_Title" "マウスジェスチャ編集")
      (dotnet:invoke w "set_Width" 400d0)
      (dotnet:invoke w "set_SizeToContent" (dotnet:static "Avalonia.Controls.SizeToContent" "Height"))
      (dotnet:invoke w "set_WindowStartupLocation"
                     (dotnet:static "Avalonia.Controls.WindowStartupLocation" "CenterOwner"))
      (dotnet:invoke w "ShowDialog" owner))))

(defun %open-menu-editor (vw)
  "メニュー構成の編集ウィンドウ (#32: NeeView 流 TreeView + ドラッグ&ドロップ + 右クリック)。
グループ=展開ノード / コマンド=子。コマンドをドラッグして別コマンド上に落とすとその直前へ、
グループ見出しに落とすとそのグループ末尾へ移動 (Avalonia intra-app DnD)。右クリックで
上へ/下へ/削除/別グループへ・グループに「コマンドを追加」。OK で *command-menu* を
menu.lisp に保存し即反映。"
  (let* ((owner (viewer-window vw))
         (w    (dotnet:new "Avalonia.Controls.Window"))
         (tree (dotnet:new "Avalonia.Controls.TreeView"))
         (mbox (list (%menu-norm *command-menu*)))
         (dstate (list nil))     ; ドラッグ中のコマンド (cons gk id)
         (dragging (list nil)))  ; 多重開始防止
    (labels
        ((groups () (car mbox))
         (upd (gk fn)
           (setf (car mbox)
                 (mapcar (lambda (g) (if (eq (first g) gk)
                                         (list (first g) (second g) (funcall fn (third g))) g))
                         (groups)))
           (refresh))
         (move-cmd (gk id dir) (upd gk (lambda (ids) (%shift-in-list ids id dir))))
         (remove-cmd (gk id)   (upd gk (lambda (ids) (remove id ids))))
         (add-cmd (gk id)      (upd gk (lambda (ids) (if (member id ids) ids (append ids (list id))))))
         (move-to-group (gk id newgk)
           (unless (eq gk newgk)
             (setf (car mbox)
                   (mapcar (lambda (g)
                             (cond ((eq (first g) gk)
                                    (list (first g) (second g) (remove id (third g))))
                                   ((eq (first g) newgk)
                                    (list (first g) (second g)
                                          (if (member id (third g)) (third g)
                                              (append (third g) (list id)))))
                                   (t g)))
                           (groups)))
             (refresh)))
         ;; --- グループ自体の操作 (#32: 追加/リネーム/削除/並べ替え + 区切り) ---
         (move-group (gk dir)            ; グループの順序を上(-1)/下(+1)へ
           (let* ((gs (groups)) (g (find gk gs :key #'first)))
             (when g (setf (car mbox) (%shift-in-list gs g dir)) (refresh))))
         (add-group ()                   ; 名前を聞いて末尾に空グループを足す
           (%prompt-text w "新規グループ名" ""
                         (lambda (name)
                           (setf (car mbox)
                                 (append (groups) (list (list (%fresh-gkey (groups)) name nil))))
                           (refresh))))
         (rename-group (gk)
           (let ((g (find gk (groups) :key #'first)))
             (when g
               (%prompt-text w "グループ名を変更" (second g)
                             (lambda (name)
                               (setf (car mbox)
                                     (mapcar (lambda (x) (if (eq (first x) gk)
                                                             (list (first x) name (third x)) x))
                                             (groups)))
                               (refresh))))))
         (delete-group (gk)
           (setf (car mbox) (remove gk (groups) :key #'first)) (refresh))
         (add-sep (gk)                   ; グループ末尾に区切り線を足す
           (upd gk (lambda (ids) (append ids (list (%make-menu-sep))))))
         ;; --- ドラッグ&ドロップ並べ替え (#32, Avalonia intra-app DnD) ---
         (drop-on-cmd (tgt-gk tgt-id)   ; コマンド行に落とす = その直前へ
           (let ((src (car dstate)))
             (when src
               (let ((sgk (car src)) (sid (cdr src)))
                 (unless (and (eq sgk tgt-gk) (eql sid tgt-id))
                   (setf (car mbox)
                         (mapcar (lambda (g)
                                   (let ((k (first g)) (ids (third g)))
                                     (cond
                                       ((and (eq k sgk) (eq k tgt-gk))
                                        (list k (second g) (%insert-before (remove sid ids) sid tgt-id)))
                                       ((eq k sgk)    (list k (second g) (remove sid ids)))
                                       ((eq k tgt-gk) (list k (second g) (%insert-before ids sid tgt-id)))
                                       (t g))))
                                 (groups)))
                   (refresh))))
             (setf (car dstate) nil)))
         (drop-on-grp (tgt-gk)          ; グループ見出しに落とす = 末尾へ移動
           (let ((src (car dstate)))
             (when src (move-to-group (car src) (cdr src) tgt-gk)))
           (setf (car dstate) nil))
         (left-pressed (item e)         ; その PointerPressed は左ボタンか
           (let ((props (dotnet:invoke (dotnet:invoke e "GetCurrentPoint" item) "get_Properties")))
             (dotnet:invoke props "get_IsLeftButtonPressed")))
         (start-drag (item gk id e)     ; 生きた PointerPressedEventArgs でその場でドラッグ開始
           ;; TreeViewItem は押下でポインタを捕捉するため PointerMoved 遅延開始は不可。
           ;; DoDragDropAsync は移動が無ければ何も起きない (純クリックは安全)。
           (unless (car dragging)
             (setf (car dragging) t (car dstate) (cons gk id))
             (let ((dt (dotnet:new "Avalonia.Input.DataTransfer")))
               (dotnet:invoke dt "Add"
                              (dotnet:static "Avalonia.Input.DataTransferItem" "CreateText" "paalam-menu"))
               (handler-case
                   (dotnet:static "Avalonia.Input.DragDrop" "DoDragDropAsync"
                                  e dt (dotnet:static "Avalonia.Input.DragDropEffects" "Move"))
                 (error (err) (log-err "menu-dnd" err))))
             (setf (car dragging) nil (car dstate) nil)))
         (setup-drag (item gk id)       ; コマンド行をドラッグ元に (左押下で開始)
           ;; Tunnel+handledEventsToo: TreeViewItem の選択処理に PointerPressed を
           ;; 食われる前に取る (CLR += では取りこぼす)。
           (%add-pressed-tunnel item
                                (lambda (s e) (declare (ignore s))
                                  (when (left-pressed item e) (start-drag item gk id e)))))
         (setup-drop (item on-drop)     ; ドロップ先 (コマンド行/グループ見出し)
           (dotnet:static "Avalonia.Input.DragDrop" "SetAllowDrop" item t)
           (%dnd-add-handler item "DragOverEvent"
                             (lambda (s e) (declare (ignore s))
                               (dotnet:invoke e "set_DragEffects"
                                              (dotnet:static "Avalonia.Input.DragDropEffects" "Move"))
                               (dotnet:invoke e "set_Handled" t))
                             t)
           (%dnd-add-handler item "DropEvent"
                             (lambda (s e) (declare (ignore s))
                               (funcall on-drop)
                               (dotnet:invoke e "set_Handled" t))
                             t))
         (mkmi (header thunk)            ; Click する MenuItem
           (let ((m (dotnet:new "Avalonia.Controls.MenuItem")))
             (dotnet:invoke m "set_Header" header)
             (dotnet:add-event m "Click" (lambda (s e) (declare (ignore s e)) (funcall thunk)))
             m))
         (mksub (header subitems)        ; サブメニュー MenuItem
           (let ((m (dotnet:new "Avalonia.Controls.MenuItem")))
             (dotnet:invoke m "set_Header" header)
             (let ((it (dotnet:invoke m "get_Items"))) (dolist (x subitems) (dotnet:invoke it "Add" x)))
             m))
         (ctx (items)                    ; ContextMenu
           (let ((cm (dotnet:new "Avalonia.Controls.ContextMenu")))
             (let ((it (dotnet:invoke cm "get_Items"))) (dolist (x items) (dotnet:invoke it "Add" x)))
             cm))
         (cmd-ctx (gk id)
           (ctx (list
                 (mkmi "上へ" (lambda () (move-cmd gk id -1)))
                 (mkmi "下へ" (lambda () (move-cmd gk id 1)))
                 (mkmi "削除" (lambda () (remove-cmd gk id)))
                 (mksub "別グループへ"
                        (mapcar (lambda (g) (mkmi (second g) (lambda () (move-to-group gk id (first g)))))
                                (remove gk (groups) :key #'first))))))
         (sep-ctx (gk id)               ; 区切り行の右クリック (上へ/下へ/削除)
           (ctx (list
                 (mkmi "上へ" (lambda () (move-cmd gk id -1)))
                 (mkmi "下へ" (lambda () (move-cmd gk id 1)))
                 (mkmi "削除" (lambda () (remove-cmd gk id))))))
         (grp-ctx (gk)
           (let ((cur (third (find gk (groups) :key #'first))))
             (ctx (list
                   (mksub "コマンドを追加"
                          (mapcar (lambda (c) (mkmi (command-label c) (lambda () (add-cmd gk (command-id c)))))
                                  (remove-if (lambda (c) (member (command-id c) cur)) *commands*)))
                   (mkmi "区切り線を追加" (lambda () (add-sep gk)))
                   (mkmi "グループを上へ" (lambda () (move-group gk -1)))
                   (mkmi "グループを下へ" (lambda () (move-group gk 1)))
                   (mkmi "グループ名を変更…" (lambda () (rename-group gk)))
                   (mkmi "グループを削除" (lambda () (delete-group gk)))))))
         (refresh ()
           (let ((items (dotnet:invoke tree "get_Items")))
             (dotnet:invoke items "Clear")
             (dolist (g (groups))
               (let ((gi (dotnet:new "Avalonia.Controls.TreeViewItem"))
                     (gk (first g)))
                 (dotnet:invoke gi "set_Header" (format nil "~a (~d)" (second g) (length (third g))))
                 (dotnet:invoke gi "set_IsExpanded" t)
                 (dotnet:invoke gi "set_ContextMenu" (grp-ctx gk))
                 (setup-drop gi (let ((k gk)) (lambda () (drop-on-grp k))))  ; グループ末尾へ
                 (let ((gci (dotnet:invoke gi "get_Items")))
                   (dolist (id (third g))
                     (let ((ci (dotnet:new "Avalonia.Controls.TreeViewItem")) (sep (%menu-sep-p id)))
                       (dotnet:invoke ci "set_Header"
                                      (if sep "──── 区切り ────"
                                          (let ((c (%command id)))
                                            (if c (command-label c) (format nil "~a" id)))))
                       (dotnet:invoke ci "set_ContextMenu" (if sep (sep-ctx gk id) (cmd-ctx gk id)))
                       (setup-drag ci gk id)
                       (setup-drop ci (let ((k gk) (i id)) (lambda () (drop-on-cmd k i))))
                       (dotnet:invoke gci "Add" ci))))
                 (dotnet:invoke items "Add" gi))))))
      (refresh)
      (dotnet:invoke tree "set_MaxHeight" 440d0)
      (dotnet:invoke tree "set_MinWidth" 340d0)
      (let* ((root (dotnet:new "Avalonia.Controls.StackPanel"))
             (hint (dotnet:new "Avalonia.Controls.TextBlock"))
             (btns (dotnet:new "Avalonia.Controls.StackPanel"))
             (addgrp-btn (%s-button "グループ追加" (lambda () (add-group))))
             (reset-btn (%s-button "既定に戻す"
                          (lambda () (setf (car mbox) (%menu-norm *default-command-menu*)) (refresh))))
             (cancel-btn (%s-button "キャンセル" (lambda () (dotnet:invoke w "Close"))))
             (ok-btn (%s-button "OK"
                       (lambda ()
                         (setf *command-menu* (%menu-denorm (groups)))
                         (save-menu) (%rebuild-menus vw) (dotnet:invoke w "Close")))))
        (dotnet:invoke hint "set_Text" "ドラッグで並べ替え (コマンド上=直前へ / グループ見出し=末尾へ)。右クリック → コマンド: 上へ/下へ/削除/別グループへ、グループ: コマンド・区切り追加/グループ上下/名前変更/削除")
        (dotnet:invoke hint "set_TextWrapping" (dotnet:static "Avalonia.Media.TextWrapping" "Wrap"))
        (dotnet:invoke hint "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 0d0 8d0))
        (dotnet:invoke btns "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
        (dotnet:invoke btns "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 12d0 0d0 0d0))
        (let ((c (dotnet:invoke btns "get_Children")))
          (dotnet:invoke c "Add" addgrp-btn) (dotnet:invoke c "Add" reset-btn)
          (dotnet:invoke c "Add" cancel-btn) (dotnet:invoke c "Add" ok-btn))
        (dotnet:invoke root "set_Margin" (dotnet:new "Avalonia.Thickness" 12d0))
        (let ((c (dotnet:invoke root "get_Children")))
          (dotnet:invoke c "Add" hint) (dotnet:invoke c "Add" tree) (dotnet:invoke c "Add" btns))
        (dotnet:invoke w "set_Content" root))
      (dotnet:invoke w "set_Title" "メニュー編集")
      (dotnet:invoke w "set_Width" 400d0)
      (dotnet:invoke w "set_SizeToContent" (dotnet:static "Avalonia.Controls.SizeToContent" "Height"))
      (dotnet:invoke w "set_WindowStartupLocation" (dotnet:static "Avalonia.Controls.WindowStartupLocation" "CenterOwner"))
      (dotnet:invoke w "ShowDialog" owner))))

