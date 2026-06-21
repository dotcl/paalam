;;;; window.lisp — paalam (#39 main.lisp split): window operations / settings-window helpers / file association
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; window / app (Avalonia)
;;; ------------------------------------------------------------------

(defvar *target-path* nil
  "Target path received by RUN-PAALAM (passed via global since the App is created by Avalonia).")

(defvar *viewers* nil
  "List of viewers for all open windows. Pushed in build-main-window,
removed on window Closed. Used to decide, when already launched via the macOS
Finder \"open\" (IActivatableLifetime), whether to open a new file in a separate
window (if empty, push onto *target-path*). On Windows/Linux the OS launches a
separate process per file, so this activation path is not taken; effectively macOS-only.")

(defun %resolve-path (path)
  "Determine the target path. Return nil if none is given (= show an empty window without opening a book).
Previously a fixed Windows path (@WLOGO_96x96.png) was used as a fallback, but it
does not exist on other OSes and open-book would throw -> initialization aborted and
no window appeared, so it was removed."
  (or (and (stringp path) (plusp (length path)) path)
      (let ((env (dotnet:static "System.Environment" "GetEnvironmentVariable" "PAALAM_PATH")))
        (and env (plusp (length env)) env))))

;;; display toggles (shared by key / menu)
(defun %toggle-spread (vw)
  (setf *spread* (not *spread*)) (%recompute vw (%cur-page vw)) (save-settings))
(defun %toggle-direction (vw)
  (setf *page-direction* (if (eq *page-direction* :rtl) :ltr :rtl))
  (show-spread vw) (save-settings))
(defun %toggle-recursive (vw)
  (setf *recursive* (not *recursive*)) (save-settings) (%reopen vw))
(defun %cycle-fit-mode (vw)
  "Cycle the fit mode whole->width->height->whole and re-fit (#15)."
  (setf *fit-mode* (ecase *fit-mode* (:whole :width) (:width :height) (:height :whole)))
  (save-settings)
  (reset-view vw)
  (%update-title vw))

(defun %open-file-dialog (vw win)
  (dotnet:static "Paalam.DialogHelper" "PickOpenBook" win
                 (dotnet:make-delegate "System.Action`1[System.String]"
                                       (lambda (p) (open-path vw p)))))
(defun %open-folder-dialog (vw win)
  (dotnet:static "Paalam.DialogHelper" "PickOpenFolder" win
                 (dotnet:make-delegate "System.Action`1[System.String]"
                                       (lambda (p) (open-path vw p)))))

;;; settings window (#9): opened in a separate window from the menu "Settings...". OK confirms and saves,
;;; Cancel discards (global special variables are not touched at all until OK is pressed). Built directly
;;; on the persistence base (*settings*/save-settings, #8); applied by redrawing according to what changed.
(defun %parse-int (s default min)
  "Parse S as an integer. Return DEFAULT if parsing fails or the value is below MIN."
  (let ((v (ignore-errors (parse-integer (string-trim '(#\Space #\Tab) s) :junk-allowed t))))
    (if (and v (>= v min)) v default)))

(defun %s-header (text)
  (let ((tb (dotnet:new "Avalonia.Controls.TextBlock")))
    (dotnet:invoke tb "set_Text" text)
    (dotnet:invoke tb "set_FontWeight" (dotnet:static "Avalonia.Media.FontWeight" "Bold"))
    (dotnet:invoke tb "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 10d0 0d0 4d0))
    tb))

(defun %s-row (label control)
  "A horizontal row [label(fixed width) ........ control]."
  (let ((row (dotnet:new "Avalonia.Controls.DockPanel"))
        (tb  (dotnet:new "Avalonia.Controls.TextBlock")))
    (dotnet:invoke tb "set_Text" label)
    (dotnet:invoke tb "set_Width" 170d0)
    (dotnet:invoke tb "set_VerticalAlignment"
                   (dotnet:static "Avalonia.Layout.VerticalAlignment" "Center"))
    (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" tb
                   (dotnet:static "Avalonia.Controls.Dock" "Left"))
    (dotnet:invoke row "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 3d0))
    (let ((c (dotnet:invoke row "get_Children")))
      (dotnet:invoke c "Add" tb)
      (dotnet:invoke c "Add" control))
    row))

(defun %set-check (cb on)
  "Set CheckBox.IsChecked (bool?) to true/false. Since #305 (setter, D1225 / 0.1.13-pkgtest3)
the bool? property setter also maps nil->false, so the boxed-false shim was removed (true=t / false=nil).
Use (dotnet:null) if indeterminate is needed."
  (dotnet:invoke cb "set_IsChecked" (if on t nil)))

(defun %s-checkbox (checked)
  (let ((cb (dotnet:new "Avalonia.Controls.CheckBox")))
    (dotnet:invoke cb "set_IsThreeState" nil)
    (%set-check cb checked) cb))

(defun %s-textbox (n)
  (let ((tb (dotnet:new "Avalonia.Controls.TextBox")))
    (dotnet:invoke tb "set_Text" (format nil "~d" n))
    (dotnet:invoke tb "set_Width" 80d0)
    (dotnet:invoke tb "set_HorizontalAlignment"
                   (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Left"))
    tb))

(defun %s-button (label click)
  (let ((b (dotnet:new "Avalonia.Controls.Button")))
    (dotnet:invoke b "set_Content" label)
    (dotnet:invoke b "set_MinWidth" 80d0)
    (dotnet:invoke b "set_Margin" (dotnet:new "Avalonia.Thickness" 6d0 0d0 0d0 0d0))
    (dotnet:add-event b "Click" (lambda (s e) (declare (ignore s e)) (funcall click)))
    b))

;;; settings combo box choices (display label . value).
(defparameter *dir-choices*    '(("Right-opening (RTL)" . :rtl) ("Left-opening (LTR)" . :ltr)))
(defparameter *fit-choices*    '(("Whole" . :whole) ("Fit width" . :width) ("Fit height" . :height)))
(defparameter *bg-choices*     '(("Black" . "Black") ("Dim gray" . "DimGray") ("Gray" . "Gray") ("White" . "White")))
(defparameter *interp-choices*
  '(("High quality (smooth)" . :high) ("Low quality" . :low) ("None (pixel art / nearest neighbor)" . :none)))
(defparameter *theme-choices*
  '(("Follow system (OS light/dark)" . :system) ("Light" . :light) ("Dark" . :dark)))
(defparameter *export-width-choices*
  '(("Original size (no conversion)" . :original) ("Max 1080px" . 1080) ("Max 1600px" . 1600) ("Max 2048px" . 2048))
  "Choices for the maximum export width (#35 ②).")

(defun %s-choice-combo (choices current &optional (test #'eql))
  "Build a ComboBox from CHOICES (label . value) and select the item matching CURRENT."
  (let ((c (dotnet:new "Avalonia.Controls.ComboBox")))
    (let ((items (dotnet:invoke c "get_Items")))
      (dolist (ch choices) (dotnet:invoke items "Add" (car ch))))
    (dotnet:invoke c "set_SelectedIndex" (or (position current choices :key #'cdr :test test) 0))
    c))

(defun %s-combo-value (combo choices)
  (cdr (nth (max 0 (dotnet:invoke combo "get_SelectedIndex")) choices)))

(defun %prompt-text (owner title initial on-ok)
  "Modal single-line text input. On OK/Enter, call (funcall on-ok input-string) and then close.
Whitespace-only input is ignored. Esc/Cancel closes. Callback style (the ShowDialog return value is not used)."
  (let* ((w    (dotnet:new "Avalonia.Controls.Window"))
         (tb   (dotnet:new "Avalonia.Controls.TextBox"))
         (root (dotnet:new "Avalonia.Controls.StackPanel"))
         (btns (dotnet:new "Avalonia.Controls.StackPanel")))
    (labels ((commit ()
               (let ((s (string-trim '(#\Space #\Tab) (or (dotnet:invoke tb "get_Text") ""))))
                 (dotnet:invoke w "Close")
                 (when (plusp (length s)) (funcall on-ok s)))))
      (dotnet:invoke tb "set_Text" (or initial ""))
      (dotnet:invoke tb "set_Width" 240d0)
      (dotnet:add-event tb "KeyDown"
        (lambda (s e) (declare (ignore s))
          (let ((kn (dotnet:invoke (dotnet:invoke e "get_Key") "ToString")))
            (cond ((or (string= kn "Return") (string= kn "Enter"))
                   (dotnet:invoke e "set_Handled" t) (commit))
                  ((string= kn "Escape")
                   (dotnet:invoke e "set_Handled" t) (dotnet:invoke w "Close"))))))
      (dotnet:invoke btns "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
      (dotnet:invoke btns "set_HorizontalAlignment" (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Right"))
      (dotnet:invoke btns "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 12d0 0d0 0d0))
      (let ((c (dotnet:invoke btns "get_Children")))
        (dotnet:invoke c "Add" (%s-button "Cancel" (lambda () (dotnet:invoke w "Close"))))
        (dotnet:invoke c "Add" (%s-button "OK" (lambda () (commit)))))
      (dotnet:invoke root "set_Margin" (dotnet:new "Avalonia.Thickness" 12d0))
      (let ((c (dotnet:invoke root "get_Children")))
        (dotnet:invoke c "Add" tb)
        (dotnet:invoke c "Add" btns))
      (dotnet:invoke w "set_Content" root)
      (dotnet:invoke w "set_Title" title)
      (dotnet:invoke w "set_SizeToContent" (dotnet:static "Avalonia.Controls.SizeToContent" "WidthAndHeight"))
      (dotnet:invoke w "set_WindowStartupLocation"
                     (dotnet:static "Avalonia.Controls.WindowStartupLocation" "CenterOwner"))
      ;; focus the input field and select all right after it is shown (swallow Focus overload differences).
      (dotnet:add-event w "Opened"
        (lambda (s e) (declare (ignore s e))
          (ignore-errors (dotnet:invoke tb "Focus") (dotnet:invoke tb "SelectAll"))))
      (dotnet:invoke w "ShowDialog" owner))))

;;; About dialog. Shown from Help -> About (Win/Linux in-window menu) or the native
;;; application menu (macOS, see %setup-mac-app-menu). Also reachable from the command palette.
(defparameter *version* "0.1"
  "Version shown in the About dialog. Keep in sync with paalam.asd :version.")
(defparameter *repository-url* "https://github.com/snmsts/paalam")

(defun %macos-p () (dotnet:static "System.OperatingSystem" "IsMacOS"))

(defun %open-url (url)
  "Open URL in the OS default browser (UseShellExecute, so it works on Win/macOS/Linux)."
  (ignore-errors
    (let ((psi (dotnet:new "System.Diagnostics.ProcessStartInfo")))
      (dotnet:invoke psi "set_FileName" url)
      (dotnet:invoke psi "set_UseShellExecute" t)
      (dotnet:static "System.Diagnostics.Process" "Start" psi))))

(defun %about-text (text &optional bold size)
  (let ((tb (dotnet:new "Avalonia.Controls.TextBlock")))
    (dotnet:invoke tb "set_Text" text)
    (when bold (dotnet:invoke tb "set_FontWeight" (dotnet:static "Avalonia.Media.FontWeight" "Bold")))
    (when size (dotnet:invoke tb "set_FontSize" (coerce size 'double-float)))
    tb))

(defun %about (&optional vw)
  "Modal About dialog: name / version / description / repository / credits.
Owner is the given viewer's window, else the first open window, else centered on screen."
  (let* ((owner (or (and vw (viewer-window vw))
                    (and *viewers* (viewer-window (first *viewers*)))))
         (w    (dotnet:new "Avalonia.Controls.Window"))
         (root (dotnet:new "Avalonia.Controls.StackPanel"))
         (btns (dotnet:new "Avalonia.Controls.StackPanel")))
    (dotnet:invoke root "set_Spacing" 6d0)
    (dotnet:invoke root "set_Margin" (dotnet:new "Avalonia.Thickness" 16d0))
    (let ((c (dotnet:invoke root "get_Children")))
      ;; app icon (the same app.ico used for the window / taskbar), shown at the top.
      (ignore-errors
        (let ((ico (concatenate 'string (dotnet:static "System.AppContext" "BaseDirectory") "app.ico")))
          (when (dotnet:static "System.IO.File" "Exists" ico)
            (let ((img (dotnet:new "Avalonia.Controls.Image")))
              (dotnet:invoke img "set_Source" (dotnet:new "Avalonia.Media.Imaging.Bitmap" ico))
              (dotnet:invoke img "set_Width" 64d0)
              (dotnet:invoke img "set_Height" 64d0)
              (dotnet:invoke img "set_HorizontalAlignment"
                             (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Center"))
              (dotnet:invoke c "Add" img)))))
      (dotnet:invoke c "Add" (%about-text "paalam" t 20))
      (dotnet:invoke c "Add" (%about-text (format nil "Version ~a" *version*)))
      (dotnet:invoke c "Add" (%about-text "A NeeView-style image viewer built with dotcl + Avalonia."))
      (dotnet:invoke c "Add" (%about-text *repository-url*))
      (dotnet:invoke c "Add" (%about-text "dotcl and Avalonia are MIT-licensed.")))
    (dotnet:invoke btns "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
    (dotnet:invoke btns "set_HorizontalAlignment" (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Right"))
    (dotnet:invoke btns "set_Spacing" 8d0)
    (dotnet:invoke btns "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 12d0 0d0 0d0))
    (let ((c (dotnet:invoke btns "get_Children")))
      (dotnet:invoke c "Add" (%s-button "GitHub" (lambda () (%open-url *repository-url*))))
      (dotnet:invoke c "Add" (%s-button "Close" (lambda () (dotnet:invoke w "Close")))))
    (dotnet:invoke (dotnet:invoke root "get_Children") "Add" btns)
    (dotnet:invoke w "set_Content" root)
    (dotnet:invoke w "set_Title" "About paalam")
    (dotnet:invoke w "set_CanResize" nil)
    (dotnet:invoke w "set_SizeToContent" (dotnet:static "Avalonia.Controls.SizeToContent" "WidthAndHeight"))
    (dotnet:invoke w "set_WindowStartupLocation"
                   (dotnet:static "Avalonia.Controls.WindowStartupLocation"
                                  (if owner "CenterOwner" "CenterScreen")))
    (dotnet:add-event w "KeyDown"
      (lambda (s e) (declare (ignore s))
        (when (string= (dotnet:invoke (dotnet:invoke e "get_Key") "ToString") "Escape")
          (dotnet:invoke e "set_Handled" t) (dotnet:invoke w "Close"))))
    (if owner
        (dotnet:invoke w "ShowDialog" owner)
        (dotnet:invoke w "Show"))))

;;; file association (#11, Windows): per-user (HKCU), so no administrator privileges needed. Register a
;;; ProgID and add it to the OpenWithProgids of the supported extensions (shows up in "Open with"). Making
;;; it the default cannot be automated per OS design, so after registering, guide the user to the
;;; "default apps" settings. Registry writes hit Microsoft.Win32.Registry directly from Lisp
;;; (policy of not adding more C# shims).
(defparameter *progid* "Paalam.Document")
(defparameter *assoc-extensions*
  (append *image-extensions* '("zip" "cbz" "rar" "cbr" "7z" "cb7" "tar" "cbt" "pdf"))
  "Extensions targeted for file association (those paalam can open).")

(defun %reg-set (path name value)
  "Write (NAME -> VALUE) into the subkey of HKCU\\PATH (created if absent). NAME=\"\" for the default value."
  (let* ((cu (dotnet:static "Microsoft.Win32.Registry" "CurrentUser"))
         (k  (dotnet:invoke cu "CreateSubKey" path)))
    (unwind-protect (dotnet:invoke k "SetValue" name value)
      (ignore-errors (dotnet:invoke k "Close")))))

(defun %assoc-key (ext) (format nil "Software\\Classes\\.~a\\OpenWithProgids" ext))

(defun %ensure-progid ()
  "Register the ProgID itself (open command + icon) (idempotent). The exe is this process."
  (let ((exe (dotnet:static "System.Environment" "ProcessPath")))
    (%reg-set (format nil "Software\\Classes\\~a" *progid*) "" "paalam image / archive / PDF")
    (%reg-set (format nil "Software\\Classes\\~a\\DefaultIcon" *progid*) "" (format nil "~a,0" exe))
    (%reg-set (format nil "Software\\Classes\\~a\\shell\\open\\command" *progid*) ""
              (format nil "\"~a\" \"%1\"" exe))))

(defun %assoc-registered-p (ext)
  "Whether *progid* is listed in EXT's OpenWithProgids."
  (handler-case
      (let* ((cu (dotnet:static "Microsoft.Win32.Registry" "CurrentUser"))
             (k  (dotnet:invoke cu "OpenSubKey" (%assoc-key ext))))
        (if k
            (unwind-protect (and (dotnet:invoke k "GetValue" *progid*) t)
              (ignore-errors (dotnet:invoke k "Close")))
            nil))
    (error () nil)))

(defun %assoc-register (exts)
  "Add EXTS as OpenWithProgids candidates for *progid* (per-user). Also prepare the ProgID itself."
  (ignore-errors (dotnet:load-assembly "Microsoft.Win32.Registry"))
  (%ensure-progid)
  (dolist (ext exts) (%reg-set (%assoc-key ext) *progid* "")))

(defun %assoc-unregister (exts)
  "Remove *progid* from EXTS' OpenWithProgids."
  (ignore-errors (dotnet:load-assembly "Microsoft.Win32.Registry"))
  (dolist (ext exts)
    (ignore-errors
      (let* ((cu (dotnet:static "Microsoft.Win32.Registry" "CurrentUser"))
             (k  (dotnet:invoke cu "OpenSubKey" (%assoc-key ext) t)))   ; writable
        (when k
          (unwind-protect
               (ignore-errors (dotnet:invoke k "DeleteValue" *progid* nil)) ; throwOnMissing=false
            (ignore-errors (dotnet:invoke k "Close"))))))))

(defun %open-default-apps ()
  "Open the Windows \"default apps\" settings (guidance, since making it the default requires user action)."
  (ignore-errors
    (let ((psi (dotnet:new "System.Diagnostics.ProcessStartInfo")))
      (dotnet:invoke psi "set_FileName" "ms-settings:defaultapps")
      (dotnet:invoke psi "set_UseShellExecute" t)
      (dotnet:static "System.Diagnostics.Process" "Start" psi))))

(defun %settings-apply (vw cb-spread cb-fps cb-rec tb-cache tb-pf tb-th
                        fit-combo bg-combo interp-combo tb-dpi tb-mem tb-slide
                        dir-img-combo dir-pdf-combo theme-combo exp-w-combo)
  "Apply the controls' current values to the globals, persist them, and redraw according to what changed."
  (let ((old-spread *spread*) (old-fps *first-page-single*)
        (old-rec *recursive*) (old-threads *loader-threads*) (old-dpi *pdf-dpi*))
    (setf *spread*            (and (dotnet:invoke cb-spread "get_IsChecked") t)
          *first-page-single* (and (dotnet:invoke cb-fps "get_IsChecked") t)
          *recursive*         (and (dotnet:invoke cb-rec "get_IsChecked") t)
          *image-direction*   (%s-combo-value dir-img-combo *dir-choices*)   ; per-type default (#33)
          *pdf-direction*     (%s-combo-value dir-pdf-combo *dir-choices*)
          ;; also update the current book's binding direction with the new per-type default (overrides any flip done with D).
          *page-direction*    (%book-direction (viewer-book vw))
          *fit-mode*          (%s-combo-value fit-combo *fit-choices*)
          *background-color*  (%s-combo-value bg-combo *bg-choices*)
          *interpolation*     (%s-combo-value interp-combo *interp-choices*)
          *theme*             (%s-combo-value theme-combo *theme-choices*)
          *export-width*      (%s-combo-value exp-w-combo *export-width-choices*)   ; #35 ②
          *pdf-dpi*           (%parse-int (dotnet:invoke tb-dpi "get_Text") *pdf-dpi* 36)
          *cache-memory-mb*   (%parse-int (dotnet:invoke tb-mem "get_Text") *cache-memory-mb* 16)
          *slideshow-interval* (%parse-int (dotnet:invoke tb-slide "get_Text") *slideshow-interval* 1)
          *cache-size*        (%parse-int (dotnet:invoke tb-cache "get_Text") *cache-size* 1)
          *prefetch-ahead*    (%parse-int (dotnet:invoke tb-pf "get_Text") *prefetch-ahead* 0)
          *loader-threads*    (%parse-int (dotnet:invoke tb-th "get_Text") *loader-threads* 1))
    (save-settings)
    (%apply-theme)               ; UI theme (follow system / light / dark; #27)
    (%apply-background vw)       ; background color (#2)
    (%apply-interpolation vw)    ; interpolation quality (#2)
    ;; in order of how heavy applying is: recursive/thread count/PDF DPI reopen the book (rebuild the cache),
    ;; spread layout changes recompute, everything else (binding direction/fit/background/interpolation/cache/prefetch) redraws.
    (cond ((or (not (eq old-rec *recursive*)) (/= old-threads *loader-threads*)
               (/= old-dpi *pdf-dpi*))
           (%reopen vw))
          ((or (not (eq old-spread *spread*)) (not (eq old-fps *first-page-single*)))
           (%recompute vw (%cur-page vw)))
          (t (show-spread vw)))))

(defparameter *modifier-keys*
  '("LeftShift" "RightShift" "LeftCtrl" "RightCtrl" "LeftAlt" "RightAlt" "LWin" "RWin")
  "Modifier keys that are not used for bindings on their own (ignored during capture while waiting for the real key).")

(defun %capture-key (owner on-key)
  "Show a small window, pass the name of the next pressed key to ON-KEY, and close. Esc=cancel; lone modifier keys are ignored."
  (let ((w  (dotnet:new "Avalonia.Controls.Window"))
        (tb (dotnet:new "Avalonia.Controls.TextBlock")))
    (dotnet:invoke tb "set_Text" "Press the key to assign (Esc to cancel)")
    (dotnet:invoke tb "set_Margin" (dotnet:new "Avalonia.Thickness" 24d0))
    (dotnet:invoke w "set_Title" "Key assignment")
    (dotnet:invoke w "set_SizeToContent"
                   (dotnet:static "Avalonia.Controls.SizeToContent" "WidthAndHeight"))
    (dotnet:invoke w "set_WindowStartupLocation"
                   (dotnet:static "Avalonia.Controls.WindowStartupLocation" "CenterOwner"))
    (dotnet:invoke w "set_Content" tb)
    (dotnet:add-event w "KeyDown"
      (lambda (s e) (declare (ignore s))
        (let ((kn (dotnet:invoke (dotnet:invoke e "get_Key") "ToString")))
          (unless (member kn *modifier-keys* :test #'string=)
            (dotnet:invoke e "set_Handled" t)
            (dotnet:invoke w "Close")
            (unless (string= kn "Escape") (funcall on-key kn))))))
    (dotnet:invoke w "ShowDialog" owner)))

(defun %goto-page (vw)
  "vi-style page navigation (#23): [count]g jumps to page count. Without count, jumps to the end
 (a fallback for direct execution from the menu/palette; for keys, G handles the end).
count is what %on-key accumulated into viewer-pending-count (1 for 1g)."
  (let* ((n (book-page-count (viewer-book vw)))
         (cnt (viewer-pending-count vw))
         (page (if cnt (1- (max 1 (min cnt n))) (1- n))))
    (when (plusp n) (%jump-to-page vw page))))

