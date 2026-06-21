;;;; dnd.lisp — paalam (#39 main.lisp 分割): ドラッグ&ドロップ (#302)
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; ドラッグ&ドロップ (#302): C# シム (Paalam.DragDropHelper) を剥がし Lisp 直叩き。
;;; Avalonia 12 の DnD は IDataTransfer + DataFormat。ルーテッドイベントは静的
;;; RoutedEvent フィールド (DragDrop.DropEvent 等) を読み、EventHandler<DragEventArgs>
;;; デリゲートで AddHandler 購読する。ファイル取り出し/パス化は静的拡張メソッド
;;; (DataTransferExtensions / StorageProviderExtensions) を直叩き。
;;; ------------------------------------------------------------------

(defun %dnd-has-files (e)
  "DragEventArgs e がファイルを含むか (DataTransferExtensions.Contains(_, DataFormat.File))。"
  (handler-case
      (let ((dt (dotnet:invoke e "get_DataTransfer")))
        (and dt
             (dotnet:static "Avalonia.Input.DataTransferExtensions" "Contains"
                            dt (dotnet:static "Avalonia.Input.DataFormat" "get_File"))
             t))
    (error (err) (log-err "dnd-has-files" err) nil)))

(defun %dnd-paths (e)
  "DragEventArgs e からドロップされたローカルパスのリストを取り出す。"
  (handler-case
      (let ((dt (dotnet:invoke e "get_DataTransfer"))
            (paths '()))
        (when dt
          (let ((files (dotnet:static "Avalonia.Input.DataTransferExtensions" "TryGetFiles" dt)))
            (when files
              (let ((en (dotnet:invoke files "GetEnumerator")))
                (loop while (dotnet:invoke en "MoveNext")
                      do (let* ((item (dotnet:invoke en "get_Current"))
                                (p (and item (dotnet:static
                                              "Avalonia.Platform.Storage.StorageProviderExtensions"
                                              "TryGetLocalPath" item))))
                           (when (and p (> (length p) 0)) (push p paths))))))))
        (nreverse paths))
    (error (err) (log-err "dnd-paths" err) nil)))

(defun %dnd-add-handler (target event-name handler &optional handled-too)
  "target (Interactive) の静的 RoutedEvent (DragDrop の event-name フィールド) に
handler=(lambda (sender args)) を購読。
AddHandler(RoutedEvent, EventHandler<DragEventArgs>, RoutingStrategies, bool)。"
  ;; 構築済みジェネリック デリゲート型は型引数を [[Type, Assembly]] と
  ;; アセンブリ修飾しないと dotcl が解決できない (単純 [Type] は corelib 型のみ; #302)。
  (dotnet:invoke target "AddHandler"
                 (dotnet:static "Avalonia.Input.DragDrop" event-name)
                 (dotnet:make-delegate
                  "System.EventHandler`1[[Avalonia.Input.DragEventArgs, Avalonia.Base]]"
                  handler)
                 "Direct, Bubble"
                 (and handled-too t)))

(defun %add-pressed-tunnel (target handler)
  "PointerPressed を Tunnel + handledEventsToo で購読 (handler=(lambda (sender args)))。
TreeViewItem 等が選択処理で PointerPressed を Handled にしても先に受け取れる
(intra-app ドラッグ開始トリガ用; #32)。CLR の += (add-event) は handledEventsToo=false
なので捕捉済みイベントを取りこぼす。"
  (dotnet:invoke target "AddHandler"
                 (dotnet:static "Avalonia.Input.InputElement" "PointerPressedEvent")
                 (dotnet:make-delegate
                  "System.EventHandler`1[[Avalonia.Input.PointerPressedEventArgs, Avalonia.Base]]"
                  handler)
                 "Tunnel"
                 t))

(defun %dnd-set-effect (e has)
  (dotnet:invoke e "set_DragEffects"
                 (dotnet:static "Avalonia.Input.DragDropEffects" (if has "Copy" "None"))))

(defun %dnd-enable (win on-drop on-highlight)
  "ウィンドウへ本ファイルの DnD を配線。on-drop=(lambda (path))、on-highlight=(lambda (bool))。
ファイルが来たら Copy エフェクト、ドロップで先頭パスを on-drop に渡す。"
  (dotnet:static "Avalonia.Input.DragDrop" "SetAllowDrop" win t)
  (%dnd-add-handler win "DragOverEvent"
                    (lambda (s e) (declare (ignore s))
                      (let ((has (%dnd-has-files e)))
                        (%dnd-set-effect e has)
                        (funcall on-highlight has))))
  (%dnd-add-handler win "DragLeaveEvent"
                    (lambda (s e) (declare (ignore s e)) (funcall on-highlight nil)))
  (%dnd-add-handler win "DropEvent"
                    (lambda (s e) (declare (ignore s))
                      (funcall on-highlight nil)
                      (let ((paths (%dnd-paths e)))
                        (when paths (funcall on-drop (first paths)))))))

(defun %dnd-enable-clip (pane on-drop on-highlight)
  "素材ペインへ DnD を配線。落ちた全パスを on-drop に渡し、e.Handled=t で
ウィンドウ側の「本を開く」DnD と区別する。"
  (dotnet:static "Avalonia.Input.DragDrop" "SetAllowDrop" pane t)
  (%dnd-add-handler pane "DragOverEvent"
                    (lambda (s e) (declare (ignore s))
                      (let ((has (%dnd-has-files e)))
                        (%dnd-set-effect e has)
                        (funcall on-highlight has)
                        (dotnet:invoke e "set_Handled" t))))
  (%dnd-add-handler pane "DragLeaveEvent"
                    (lambda (s e) (declare (ignore s))
                      (funcall on-highlight nil)
                      (dotnet:invoke e "set_Handled" t)))
  (%dnd-add-handler pane "DropEvent"
                    (lambda (s e) (declare (ignore s))
                      (funcall on-highlight nil)
                      (dolist (p (%dnd-paths e)) (funcall on-drop p))
                      (dotnet:invoke e "set_Handled" t))))

