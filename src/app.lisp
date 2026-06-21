;;;; app.lisp — paalam (#39 main.lisp split): build-main-window, App emit, run-paalam
(in-package :paalam)

(defun build-main-window (&optional (target *target-path*))
  "Open the book at TARGET (default *target-path*) and return a spread-capable
Avalonia Window (built on the calling thread). Two Images meet at the center of a
2-column Grid. TARGET is parameterized so open-in-new-window can make another window from any path."
  (multiple-value-bind (book start) (open-book (%resolve-path target))
    (setf *page-direction* (%book-direction book))   ; binding direction by type (#33)
    (let* ((win   (dotnet:new "Avalonia.Controls.Window"))
           (vport (dotnet:new "Avalonia.Controls.Border"))   ; viewport for clipping
           (stage (dotnet:new "Avalonia.Controls.Canvas"))   ; spread stage (coords computed by us; #15)
           (li    (dotnet:new "Avalonia.Controls.Image"))
           (ri    (dotnet:new "Avalonia.Controls.Image"))
           (rot   (dotnet:new "Avalonia.Media.RotateTransform"))   ; rotation (#28)
           (scale (dotnet:new "Avalonia.Media.ScaleTransform"))
           (trans (dotnet:new "Avalonia.Media.TranslateTransform"))
           (grp   (dotnet:new "Avalonia.Media.TransformGroup"))
           (spreads (compute-spreads (book-page-count book)))
           (path  (%resolve-path target))
           ;; resume from the recorded page if history exists (#7).
           (start2 (let ((r (history-page path)))
                     (if r (max 0 (min r (1- (book-page-count book)))) start)))
           (vw   (make-viewer :book book :spreads spreads
                              :sindex (spread-index-of spreads start2)
                              :source path
                              :window win :left-img li :right-img ri
                              :viewport vport :scale-xf scale :trans-xf trans :rot-xf rot)))
      ;; place on the Canvas at native size (coords computed by %render-spread). Stretch=None.
      (dolist (im (list li ri))
        (dotnet:invoke im "set_Stretch" (dotnet:static "Avalonia.Media.Stretch" "None")))
      (let ((children (dotnet:invoke stage "get_Children")))
        (dotnet:invoke children "Add" li)
        (dotnet:invoke children "Add" ri))
      ;; RenderTransform on stage (effective scale = fit base x zoom + pan). Origin = top-left,
      ;; so screen = scale*canvas-coord + translate composes simply.
      (let ((gc (dotnet:invoke grp "get_Children")))
        (dotnet:invoke gc "Add" rot)       ; order: rotate -> scale -> translate (#28)
        (dotnet:invoke gc "Add" scale)
        (dotnet:invoke gc "Add" trans))
      (dotnet:invoke stage "set_RenderTransform" grp)
      (dotnet:invoke stage "set_RenderTransformOrigin"
                     (dotnet:static "Avalonia.RelativePoint" "TopLeft"))
      ;; viewport (clip) > stage
      ;; Keep it Focusable; by placing focus here at startup / after menu operations,
      ;; keys like the arrows always reach the Window's KeyDown (not swallowed by the menu).
      (dotnet:invoke vport "set_Focusable" t)
      (dotnet:invoke vport "set_ClipToBounds" t)
      (dotnet:invoke vport "set_Child" stage)
      ;; recompute base scale / anchor on resize (#15).
      (dotnet:add-event vport "SizeChanged"
                        (lambda (s e) (declare (ignore s e)) (%apply-fit vw)))
      ;; root: [viewport* | material pane (Auto, hidden by default)] (#18 Phase 2)
      (let* ((root  (dotnet:new "Avalonia.Controls.Grid"))
             (btree (dotnet:new "Avalonia.Controls.TreeView"))      ; folder tree (#6 B)
             (tpane (dotnet:new "Avalonia.Controls.ScrollViewer"))  ; page-thumbnail shelf (#6 A)
             (tstack (dotnet:new "Avalonia.Controls.StackPanel"))
             (pane  (dotnet:new "Avalonia.Controls.DockPanel"))
             (ebtn  (dotnet:new "Avalonia.Controls.Button"))
             (pbtn  (dotnet:new "Avalonia.Controls.Button"))   ; preview (open clip as a book; #35 ⑤)
             (scr   (dotnet:new "Avalonia.Controls.ScrollViewer"))
             (stack (dotnet:new "Avalonia.Controls.StackPanel"))
             (overlay (dotnet:new "Avalonia.Controls.Border")))   ; DnD drop-eligibility highlight frame (#20)
        ;; 4 columns: [folder(Auto) | thumbs(Auto) | viewport(star) | material(Auto)]. Auto collapses when hidden.
        (let ((cols (dotnet:invoke root "get_ColumnDefinitions")))
          (dolist (w (list "Auto" "Auto" "*" "Auto"))
            (let ((c (dotnet:new "Avalonia.Controls.ColumnDefinition")))
              (unless (string= w "*")
                (dotnet:invoke c "set_Width" (dotnet:static "Avalonia.Controls.GridLength" "Auto")))
              (dotnet:invoke cols "Add" c))))
        ;; folder-tree pane (leftmost, hidden by default). select = expand/navigate, double-click = open.
        (dotnet:invoke btree "set_Width" 220d0)
        (dotnet:invoke btree "set_IsVisible" nil)
        (dotnet:invoke btree "set_Background" (dotnet:static "Avalonia.Media.Brushes" "DimGray"))
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" btree 0)
        (setf (viewer-browse-pane vw) btree (viewer-browse-tree vw) btree)
        (dotnet:add-event btree "SelectionChanged"
          (lambda (s e) (declare (ignore s e))
            (let ((item (dotnet:invoke btree "get_SelectedItem")))
              (when item
                (let ((path (dotnet:invoke item "get_Tag")))
                  (when (and path (dotnet:static "System.IO.Directory" "Exists" path))
                    (%browse-populate item path)
                    (dotnet:invoke item "set_IsExpanded" t)))))))
        (dotnet:add-event btree "DoubleTapped"
          (lambda (s e) (declare (ignore s e))
            (let ((item (dotnet:invoke btree "get_SelectedItem")))
              (when item
                (let ((path (dotnet:invoke item "get_Tag")))
                  (when path (ignore-errors (open-path vw path)) (%focus-viewport vw)))))))
        ;; thumbnail pane (hidden by default)
        (dotnet:invoke tpane "set_Width" 180d0)
        (dotnet:invoke tpane "set_IsVisible" nil)
        (dotnet:invoke tpane "set_Background" (dotnet:static "Avalonia.Media.Brushes" "DimGray"))
        (dotnet:invoke tpane "set_Content" tstack)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" tpane 1)
        (setf (viewer-thumb-pane vw) tpane (viewer-thumb-stack vw) tstack)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" vport 2)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" pane  3)
        (dotnet:invoke pane "set_Width" 220d0)
        (dotnet:invoke pane "set_IsVisible" nil)
        (dotnet:invoke pane "set_Background" (dotnet:static "Avalonia.Media.Brushes" "DimGray"))
        ;; DnD highlight frame: a thick border covering the whole area (transparent inside). Disable hit-testing so it does not block the drop.
        (dotnet:invoke overlay "set_BorderBrush" (dotnet:static "Avalonia.Media.Brushes" "DeepSkyBlue"))
        (dotnet:invoke overlay "set_BorderThickness" (dotnet:new "Avalonia.Thickness" 4d0))
        (dotnet:invoke overlay "set_IsHitTestVisible" nil)
        (dotnet:invoke overlay "set_IsVisible" nil)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" overlay 0)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumnSpan" overlay 4)
        (dotnet:invoke ebtn "set_Content" "Export")
        (dotnet:add-event ebtn "Click" (lambda (s e) (declare (ignore s e)) (%pane-export vw)))
        (dotnet:invoke pbtn "set_Content" "Preview")
        (dotnet:add-event pbtn "Click" (lambda (s e) (declare (ignore s e)) (%open-clip-as-book vw)))
        (dotnet:invoke scr "set_Content" stack)
        (setf (viewer-clip-scroll vw) scr)   ; for cursor-following scroll (#35 ④)
        ;; vi keys for clip editing (#35 ④ Phase 1). Local handler active only while stack has focus.
        ;; Handled=t keeps them from leaking to the window's %on-key (P=pane / D=direction, etc.) (avoids key conflicts).
        (dotnet:invoke stack "set_Focusable" t)
        (dotnet:add-event stack "KeyDown"
          (lambda (s e) (declare (ignore s))
            (let* ((kn   (dotnet:invoke (dotnet:invoke e "get_Key") "ToString"))
                   (d    (%digit-key kn))
                   (cnt  (viewer-clip-count vw))
                   (pend (viewer-clip-pending vw)))
              (flet ((h () (dotnet:invoke e "set_Handled" t))
                     (clear () (setf (viewer-clip-count vw) nil (viewer-clip-pending vw) nil)))
                (cond
                  ;; accumulate count (1-9 starts / 0 while continuing)
                  ((and d (or (> d 0) cnt))
                   (h) (setf (viewer-clip-pending vw) nil
                             (viewer-clip-count vw) (+ (* 10 (or cnt 0)) d)))
                  ((member kn '("J" "Down") :test #'string=) (h) (%clip-cursor-move vw (or cnt 1)) (clear))
                  ((member kn '("K" "Up")   :test #'string=) (h) (%clip-cursor-move vw (- (or cnt 1))) (clear))
                  ;; dd = cut (double press). count is kept on the first press.
                  ((string= kn "D") (h) (if (eq pend :d) (progn (%clip-cut vw cnt) (clear))
                                           (setf (viewer-clip-pending vw) :d)))
                  ;; yy = copy (double press)
                  ((string= kn "Y") (h) (if (eq pend :y) (progn (%clip-yank vw cnt) (clear))
                                           (setf (viewer-clip-pending vw) :y)))
                  ;; x = cut (single / count / selection)
                  ((member kn '("X" "Delete") :test #'string=) (h) (%clip-cut vw cnt) (clear))
                  ;; p = paste after / P(Shift) = paste before (can transplant to another clip)
                  ((string= kn "P") (h) (%clip-paste vw (%shift? e)) (clear))
                  ;; Space = toggle selection
                  ((string= kn "Space") (h) (%clip-toggle-select vw) (clear))
                  ((string= kn "Escape")
                   (h) (if (or (viewer-clip-selected vw) pend cnt)
                           (progn (setf (viewer-clip-selected vw) nil) (clear) (%clip-cursor-highlight vw))
                           (%focus-viewport vw)))
                  ;; unknown keys: just reset state and pass through (keep globals like F11 alive)
                  (t (clear)))))))
        (let ((pc (dotnet:invoke pane "get_Children"))
              (cbar (%build-clip-bar vw))           ; clip-selection bar (#35 ③)
              (actions (dotnet:new "Avalonia.Controls.StackPanel")))   ; [Preview][Export] (#35 ⑤)
          (dotnet:invoke actions "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
          (let ((ac (dotnet:invoke actions "get_Children")))
            (dotnet:invoke ac "Add" pbtn) (dotnet:invoke ac "Add" ebtn))
          (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" cbar
                         (dotnet:static "Avalonia.Controls.Dock" "Top"))
          (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" actions
                         (dotnet:static "Avalonia.Controls.Dock" "Top"))
          (dotnet:invoke pc "Add" cbar)
          (dotnet:invoke pc "Add" actions)
          (dotnet:invoke pc "Add" scr))
        (let ((rc (dotnet:invoke root "get_Children")))
          (dotnet:invoke rc "Add" btree)
          (dotnet:invoke rc "Add" tpane)
          (dotnet:invoke rc "Add" vport)
          (dotnet:invoke rc "Add" pane)
          (dotnet:invoke rc "Add" overlay)   ; added last = frontmost
          ;; : command bar (spans all columns, bottom, frontmost; #23)
          (let ((cb (%make-cmdbar vw)))
            (dotnet:static "Avalonia.Controls.Grid" "SetColumn" cb 0)
            (dotnet:static "Avalonia.Controls.Grid" "SetColumnSpan" cb 4)
            (dotnet:invoke rc "Add" cb))
          ;; status display (spans all columns, top-center, frontmost; messages stay visible even in fullscreen; #23)
          (let ((st (%make-status vw)))
            (dotnet:static "Avalonia.Controls.Grid" "SetColumn" st 0)
            (dotnet:static "Avalonia.Controls.Grid" "SetColumnSpan" st 4)
            (dotnet:invoke rc "Add" st)))
        (setf (viewer-clip-pane vw) pane (viewer-clip-stack vw) stack
              (viewer-dnd-overlay vw) overlay)
        ;; add material via DnD onto the pane (#18 Phase 2b). Emphasize the pane background during DnD (#20).
        (%dnd-enable-clip pane
                          (lambda (p) (%clip-drop vw p))
                          (lambda (on)
                            (dotnet:invoke pane "set_Background"
                                           (dotnet:static "Avalonia.Media.Brushes"
                                                          (if on "SteelBlue" "DimGray")))))
        ;; menu bar (#21): wrap Window.Content in a DockPanel [Menu(Top) | root].
        (dotnet:invoke vport "set_ContextMenu" (%build-context-menu vw))   ; right-click menu (#26; includes gesture suppression #29)
        (let ((dock (dotnet:new "Avalonia.Controls.DockPanel"))
              (menu (%build-menu vw win)))
          (setf (viewer-menu vw) menu)
          (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" menu
                         (dotnet:static "Avalonia.Controls.Dock" "Top"))
          (let ((dc (dotnet:invoke dock "get_Children")))
            (dotnet:invoke dc "Add" menu)
            (dotnet:invoke dc "Add" root))
          (dotnet:invoke win "set_Content" dock)))
      (%apply-background vw)          ; background color (= margin/letterbox, #2)
      (%apply-interpolation vw)       ; upscale interpolation quality (#2)
      ;; window / taskbar icon (app.ico right under the output dir).
      (ignore-errors
        (let ((ico (concatenate 'string (dotnet:static "System.AppContext" "BaseDirectory") "app.ico")))
          (when (dotnet:static "System.IO.File" "Exists" ico)
            (dotnet:invoke win "set_Icon" (dotnet:new "Avalonia.Controls.WindowIcon" ico)))))
      (dotnet:invoke win "set_Width"  1200d0)
      (dotnet:invoke win "set_Height" 800d0)
      (dotnet:add-event win "KeyDown"
                        (lambda (s e) (declare (ignore s)) (%on-key vw e)))
      ;; focus the viewport on show so keys work right from startup.
      (dotnet:add-event win "Opened"
                        (lambda (s e) (declare (ignore s e)) (%focus-viewport vw)))
      (dotnet:add-event win "PointerWheelChanged"
                        (lambda (s e) (declare (ignore s)) (%on-wheel vw e)))
      (dotnet:add-event win "PointerPressed"
                        (lambda (s e) (declare (ignore s)) (%on-press vw e)))
      (dotnet:add-event win "PointerMoved"
                        (lambda (s e) (declare (ignore s)) (%on-move vw e)))
      (dotnet:add-event win "PointerReleased"
                        (lambda (s e) (declare (ignore s)) (%on-release vw e)))
      (dotnet:add-event win "Closed"
                        (lambda (s e) (declare (ignore s e))
                          (ignore-errors (save-history))   ; persist the last page (#7)
                          (ignore-errors (loader-stop (viewer-loader vw)))
                          ;; close the "current book" even after replace via open-path (not the initial book).
                          (ignore-errors (book-close (viewer-book vw)))
                          ;; multi-window: drop this viewer from the list.
                          (setf *viewers* (remove vw *viewers*))))
      ;; open another book via drag & drop (#19). Wiring is contained in the C# shim.
      ;; show a highlight frame over the whole window during DnD (#20).
      (%dnd-enable win
                   (lambda (p) (open-path vw p))
                   (lambda (on) (dotnet:invoke (viewer-dnd-overlay vw) "set_IsVisible" (and on t))))
      ;; start the read-ahead loader (after vw is created, since the closure references vw).
      (setf (viewer-loader vw)
            (open-loader (let ((p path)) (lambda () (open-book p)))
                         (lambda (index)
                           (%ui-post (lambda () (%on-page-loaded vw index))))))
      (show-spread vw)
      (push vw *viewers*)   ; multi-window: register the open viewer (removed on Closed)
      win)))

(defun open-in-new-window (path)
  "Open PATH in a new window (call on the UI thread). Used by macOS activation to
open the second and later files in separate windows. The first window is shown via
the lifetime's set_MainWindow, but later ones need an explicit Show."
  (let ((win (build-main-window path)))
    (dotnet:invoke win "Show")
    win))

(defun %activate-open (path)
  "Open a PATH that arrived via macOS Finder \"Open\" / Dock drop (on the UI thread).
- if there is an empty window with nothing open yet (viewer-source = nil), replace it
  (so an un-launched -> one-file case does not end up with both an empty window and a file window).
- if there is no empty window, open in a new window (multiple files arrive one at a time).
- before the first build (*viewers* empty), stack it onto *target-path* (the startup sequence opens it)."
  (cond
    ((null *viewers*) (setf *target-path* path))
    (t (let ((empty (find-if (lambda (vw) (null (viewer-source vw))) *viewers*)))
         (if empty
             (open-path empty path)
             (open-in-new-window path))))))

(defun %maybe-autoclose (life)
  "Debug only: only when PAALAM_DEBUG is enabled, call lifetime.Shutdown after
DOTCL_AUTOCLOSE_MS (positive ms) (auto-close for startup verification). So a production
launch (Finder/launchd, etc.) does not exit immediately if a stale DOTCL_AUTOCLOSE_MS
is left in the environment, it acts only when both *debug* and a \"positive integer ms\" hold."
  (when *debug*
    (let ((s (dotnet:static "System.Environment" "GetEnvironmentVariable" "DOTCL_AUTOCLOSE_MS")))
      (when (and s (plusp (length s)))
        (let ((ms (parse-integer s :junk-allowed t))
              (timer (dotnet:new "Avalonia.Threading.DispatcherTimer")))
          (when (and ms (plusp ms))   ; does not fire for 0, negative, or non-numeric
            (dotnet:invoke timer "set_Interval"
                           (dotnet:static "System.TimeSpan" "FromMilliseconds"
                                          (coerce ms 'double-float)))
            (dotnet:add-event timer "Tick"
                              (lambda (sndr e) (declare (ignore sndr e))
                                (dotnet:invoke timer "Stop")
                                (dotnet:invoke life "Shutdown" 0)))
            (dotnet:invoke timer "Start")))))))

(defun %setup-mac-app-menu (app)
  "macOS: add an About item to the native application menu (the bold app-name menu).
A bare NativeMenuItem (no child menu) added to the app-level NativeMenu lands in the
application menu on macOS. Win/Linux use the in-window Help menu instead (see %add-help-menu)."
  (handler-case
      (let* ((about (dotnet:new "Avalonia.Controls.NativeMenuItem" "About paalam"))
             (menu  (dotnet:new "Avalonia.Controls.NativeMenu")))
        (dotnet:add-event about "Click"
                          (lambda (s e) (declare (ignore s e))
                            (%about (first *viewers*))))
        (dotnet:invoke (dotnet:invoke menu "get_Items") "Add" about)
        (dotnet:static "Avalonia.Controls.NativeMenu" "SetMenu" app menu))
    (error (e) (log-err "mac-app-menu" e))))

;;; emit an Avalonia.Application subclass on the Lisp side.
(dotnet:define-class "Paalam.App" ("Avalonia.Application")
  (:ctor ()
    ;; Fluent theme (for future templated controls like menus / toolbars).
    ;; In 0.1.11 dotnet:new can fill an all-optional-arg ctor, so no args is OK (#292 resolved).
    (handler-case
        (let ((styles (dotnet:invoke self "get_Styles"))
              (theme (dotnet:new "Avalonia.Themes.Fluent.FluentTheme")))
          (dotnet:invoke styles "Add" theme)
          (%apply-theme))              ; apply OS-follow or fixed theme (#27)
      (error (e) (log-err "ctor" e))))
  (:methods
    ("OnFrameworkInitializationCompleted" () :returns Void :override t
      (handler-case
          (let ((life (dotnet:invoke self "get_ApplicationLifetime"))
                (win  (build-main-window)))
            (dotnet:invoke life "set_MainWindow" win)
            (when (%macos-p) (%setup-mac-app-menu self))   ; About in the native app menu (macOS)
            ;; receive files arriving via macOS Finder "Open" / Dock drop / file-association launch.
            ;; replace if there is a viewer; otherwise stack onto the target opened by the next build.
            (dotnet:static "Paalam.FileOpenHelper" "Enable" self
                           (dotnet:make-delegate "System.Action`1[System.String]"
                             (lambda (p)
                               (%ui-post (lambda () (%activate-open p))))))
            (%maybe-autoclose life))
        (error (e) (log-err "init" e))))))

(defun run-paalam (&optional target)
  "Host entry point. Drives AppBuilder.Configure<Paalam.App>().UsePlatformDetect()
   .StartWithClassicDesktopLifetime(args) (blocks until the window closes)."
  (setf *debug* (let ((e (dotnet:static "System.Environment" "GetEnvironmentVariable" "PAALAM_DEBUG")))
                  (and e (plusp (length e)) t)))
  (load-settings)
  (load-keybinds)
  (load-menu)
  (load-history)
  (load-bookmarks)
  (load-clip)
  (load-clip-cache)                    ; #35 ①: local cache catalog for clips
  (%clip-cache-warm)                   ; fill uncached entries of existing clips in the background
  (load-gestures)
  (setf *target-path* target)
  (let* ((b    (dotnet:static-generic "Avalonia.AppBuilder" "Configure" (list "Paalam.App")))
         (b    (dotnet:static "Avalonia.AppBuilderDesktopExtensions" "UsePlatformDetect" b))
         ;; macOS: pin to Software rendering to avoid CVDisplayLink RenderTimer startup failure
         ;; (-6661, Avalonia #14927) (a fix for -6661 reproduced by Finder launch / lid close, etc.).
         (b    (if (dotnet:static "System.OperatingSystem" "IsMacOS")
                   (dotnet:static "Paalam.MacRenderFix" "UseSoftwareRendering" b)
                   b))
         (args (dotnet:static-generic "System.Array" "Empty" (list "System.String"))))
    (dotnet:static "Avalonia.ClassicDesktopStyleApplicationLifetimeExtensions"
                   "StartWithClassicDesktopLifetime"
                   b args
                   (dotnet:static "Avalonia.Controls.ShutdownMode" "OnLastWindowClose"))))
