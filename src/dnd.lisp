;;;; dnd.lisp — paalam (#39 main.lisp split): drag and drop (#302)
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; drag and drop (#302): remove the C# shim (Paalam.DragDropHelper) and call Lisp directly.
;;; Avalonia 12 DnD is IDataTransfer + DataFormat. For routed events, read the static
;;; RoutedEvent fields (DragDrop.DropEvent etc.) and subscribe via AddHandler with an
;;; EventHandler<DragEventArgs> delegate. File extraction / path conversion call the static
;;; extension methods (DataTransferExtensions / StorageProviderExtensions) directly.
;;; ------------------------------------------------------------------

(defun %dnd-has-files (e)
  "Whether DragEventArgs e contains files (DataTransferExtensions.Contains(_, DataFormat.File))."
  (handler-case
      (let ((dt (dotnet:invoke e "get_DataTransfer")))
        (and dt
             (dotnet:static "Avalonia.Input.DataTransferExtensions" "Contains"
                            dt (dotnet:static "Avalonia.Input.DataFormat" "get_File"))
             t))
    (error (err) (log-err "dnd-has-files" err) nil)))

(defun %dnd-paths (e)
  "Extract the list of dropped local paths from DragEventArgs e."
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
  "Subscribe handler=(lambda (sender args)) to target's (Interactive) static RoutedEvent
(the event-name field of DragDrop).
AddHandler(RoutedEvent, EventHandler<DragEventArgs>, RoutingStrategies, bool)."
  ;; A constructed generic delegate type cannot be resolved by dotcl unless its type
  ;; argument is assembly-qualified as [[Type, Assembly]] (plain [Type] resolves corelib types only; #302).
  (dotnet:invoke target "AddHandler"
                 (dotnet:static "Avalonia.Input.DragDrop" event-name)
                 (dotnet:make-delegate
                  "System.EventHandler`1[[Avalonia.Input.DragEventArgs, Avalonia.Base]]"
                  handler)
                 "Direct, Bubble"
                 (and handled-too t)))

(defun %add-pressed-tunnel (target handler)
  "Subscribe to PointerPressed with Tunnel + handledEventsToo (handler=(lambda (sender args))).
Even when a TreeViewItem etc. marks PointerPressed as Handled during selection, we can still
receive it first (for the intra-app drag-start trigger; #32). The CLR's += (add-event) uses
handledEventsToo=false, so it would miss already-handled events."
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
  "Wire up book-file DnD to the window. on-drop=(lambda (path)), on-highlight=(lambda (bool)).
When files arrive, set the Copy effect; on drop, pass the first path to on-drop."
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
  "Wire up DnD to the material pane. Pass all dropped paths to on-drop, and use e.Handled=t to
distinguish it from the window's \"open book\" DnD."
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

