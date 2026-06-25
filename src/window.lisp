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
(defparameter *repository-url* "https://github.com/snmsts/paalam")

(defun %system-slot (reader fallback)
  "Read a slot of the paalam ASDF system. Falls back to FALLBACK when the system isn't registered
(distributed binary) or the slot is empty. A list value collapses to its first element."
  (or (ignore-errors
        (let ((v (funcall reader (asdf:find-system "paalam"))))
          (let ((v (if (consp v) (first v) v)))
            (and (stringp v) (plusp (length v)) v))))
      fallback))

;;; Fallback library list for the About dialog. The primary source is paalam.deps.json (see
;;; %deps-from-json), which tracks the csproj automatically; this curated list is only used when
;;; that file can't be read. Entries are assembly simple names (= DLL names) whose versions are
;;; read from the actual binaries via reflection (%assembly-version).
(defparameter *dependencies*
  '("DotCL.Runtime" "Avalonia" "SkiaSharp" "SharpCompress" "PDFtoImage" "MetadataExtractor")
  "Fallback assembly names shown (with reflected versions) when paalam.deps.json is unavailable.")

(defun %assembly-fvi (name)
  "FileVersionInfo of assembly NAME (loaded by simple name), or NIL if it can't be resolved."
  (ignore-errors
    (let* ((asm (dotnet:static "System.Reflection.Assembly" "Load" name))
           (loc (dotnet:invoke asm "get_Location")))
      (when (and loc (plusp (length loc)))
        (dotnet:static "System.Diagnostics.FileVersionInfo" "GetVersionInfo" loc)))))

(defun %assembly-version (name)
  "ProductVersion (= NuGet package version) of assembly NAME, or NIL. ProductVersion carries a
trailing \"+<commit>\" build suffix, which we strip."
  (let ((fvi (%assembly-fvi name)))
    (when fvi
      (let ((pv (ignore-errors (dotnet:invoke fvi "get_ProductVersion"))))
        (when (and pv (plusp (length pv)))
          (subseq pv 0 (or (position #\+ pv) (length pv))))))))

(defun %assembly-copyright (name)
  "LegalCopyright of assembly NAME (e.g. \"Copyright © 2026 …\"), or NIL if empty."
  (let ((fvi (%assembly-fvi name)))
    (when fvi
      (let ((cr (let ((c (ignore-errors (dotnet:invoke fvi "get_LegalCopyright"))))
                  (and c (string-trim " " c)))))
        (when (and cr (plusp (length cr))) cr)))))

;;; paalam's own metadata, read from its binary (csproj <Version>/<Copyright>) just like the
;;; dependency rows — except the license, which has no FileVersionInfo field and stays in the asd
;;; (:license). Fallbacks cover an unstamped/dev binary.
(defparameter *version*
  (or (%assembly-version "paalam")
      (ignore-errors (asdf:component-version (asdf:find-system "paalam")))
      "0.1")
  "Version shown in the About dialog. From the paalam assembly (csproj <Version>); asd :version is the fallback.")
(defparameter *copyright*
  (or (%assembly-copyright "paalam") "Copyright © 2026 SANO Masatoshi")
  "Copyright shown in the About dialog. From the paalam assembly LegalCopyright (csproj <Copyright>).")
(defparameter *license* (%system-slot 'asdf:system-license "MIT")
  "paalam's own license, shown in the About dialog. From paalam.asd :license (no DLL field carries it).")

(defun %deps-from-json ()
  "Direct package dependencies as (NAME . VERSION), read from paalam.deps.json next to the app.
That file is the build's own record of the csproj PackageReferences (and ships with every
build, dev or distributed), so the About list tracks the project automatically — no hand-kept
name list. Avalonia.* sub-packages collapse into the single base \"Avalonia\" row. NIL if the
file is missing or its shape doesn't match (caller then falls back to reflection).

Parsed with .NET Regex rather than System.Text.Json: Match/Group are reference types and avoid
the value-type JSON enumerators that dotcl marshals poorly. The app's dependency object is
  \"paalam/<ver>\": { \"dependencies\": { \"Name\": \"Version\", ... }, ... }
and dependency values never contain '}', so [^}]* safely captures just that inner object."
  (ignore-errors
    (let* ((base (dotnet:static "System.AppContext" "BaseDirectory"))
           (path (concatenate 'string base "paalam.deps.json")))
      (when (%file-exists path)
        (let* ((text  (dotnet:static "System.IO.File" "ReadAllText" path))
               (rx    "System.Text.RegularExpressions.Regex")
               (block (dotnet:static rx "Match" text
                        "\"paalam/[^\"]*\":\\s*\\{\\s*\"dependencies\":\\s*\\{([^}]*)\\}")))
          (when (dotnet:invoke block "get_Success")
            (let* ((inner (dotnet:invoke (dotnet:invoke (dotnet:invoke block "get_Groups")
                                                        "get_Item" 1) "get_Value"))
                   (ms    (dotnet:static rx "Matches" inner
                            "\"([^\"]+)\"\\s*:\\s*\"([^\"]+)\""))
                   (n     (dotnet:invoke ms "get_Count"))
                   (out   '()))
              (dotimes (i n)
                (let* ((g    (dotnet:invoke (dotnet:invoke ms "get_Item" i) "get_Groups"))
                       (name (dotnet:invoke (dotnet:invoke g "get_Item" 1) "get_Value"))
                       (ver  (dotnet:invoke (dotnet:invoke g "get_Item" 2) "get_Value")))
                  (unless (eql 0 (search "Avalonia." name))   ; collapse Avalonia.* → Avalonia
                    (push (cons name ver) out))))
              (nreverse out))))))))

(defun %assembly-company (name)
  "CompanyName (= the library's author/vendor) of assembly NAME, or NIL. Read from the DLL's
FileVersionInfo (deps.json carries no author metadata, the binary does). The .NET SDK's
\"Your Company\" placeholder (e.g. SkiaSharp.Heic ships it) is dropped — an empty author column
reads better than a fake vendor name."
  (let ((fvi (%assembly-fvi name)))
    (when fvi
      (let ((co (let ((c (ignore-errors (dotnet:invoke fvi "get_CompanyName"))))
                  (and c (string-trim " " c)))))
        (when (and co (plusp (length co)) (not (string-equal co "Your Company")))
          co)))))

(defun %about-deps ()
  "List of (NAME VERSION AUTHOR) for the About dialog. Name/version come from paalam.deps.json
\(auto, tracks the csproj PackageReferences); author is the DLL's CompanyName. Falls back to
reflecting versions off the curated *dependencies* when deps.json is unavailable."
  (mapcar (lambda (nv)
            (list (car nv) (cdr nv) (or (%assembly-company (car nv)) "")))
          (or (%deps-from-json)
              (loop for name in *dependencies*
                    for v = (%assembly-version name)
                    when v collect (cons name v)))))

(defun %deps-row (name ver author &key bold stripe)
  "One ListBoxItem with three shared-size columns (library / version / author). BOLD for the
header row; STRIPE (a brush) shades the row. Static credits list, so it isn't hit-testable."
  (let* ((grid (dotnet:new "Avalonia.Controls.Grid"))
         (cols (dotnet:invoke grid "get_ColumnDefinitions"))
         (kids (dotnet:invoke grid "get_Children"))
         (item (dotnet:new "Avalonia.Controls.ListBoxItem")))
    (flet ((addcol (group)
             (let ((c (dotnet:new "Avalonia.Controls.ColumnDefinition")))
               (dotnet:invoke c "set_Width" (dotnet:static "Avalonia.Controls.GridLength" "Auto"))
               (dotnet:invoke c "set_SharedSizeGroup" group)   ; align columns across all rows
               (dotnet:invoke cols "Add" c)))
           (cell (text col muted)
             (let ((tb (%about-text text bold)))
               (dotnet:static "Avalonia.Controls.Grid" "SetColumn" tb col)
               (when (plusp col)                                ; gap between columns
                 (dotnet:invoke tb "set_Margin" (dotnet:new "Avalonia.Thickness" 18d0 0d0 0d0 0d0)))
               (when muted
                 (dotnet:invoke tb "set_Foreground" (dotnet:static "Avalonia.Media.Brushes" "Gray")))
               (dotnet:invoke kids "Add" tb))))
      (addcol "lib") (addcol "ver") (addcol "auth")
      (cell name   0 nil)
      (cell ver    1 (not bold))                                ; version/author muted on data rows
      (cell author 2 (not bold)))
    (dotnet:invoke item "set_Content" grid)
    (dotnet:invoke item "set_IsHitTestVisible" nil)
    (when stripe (dotnet:invoke item "set_Background" stripe))
    item))

(defun %about-deps-list (deps)
  "A striped three-column (library / version / author) ListBox of bundled libraries, with a
header row. Columns align via Grid SharedSizeScope. Odd data rows get a faint translucent grey
so the stripe reads on both light and dark themes."
  (let* ((lb     (dotnet:new "Avalonia.Controls.ListBox"))
         (items  (dotnet:invoke lb "get_Items"))
         ;; Built via set_Color/set_Opacity (single-overload setters) rather than the
         ;; SolidColorBrush(Color) ctor: dotcl mis-resolves that ctor to SolidColorBrush(uint)
         ;; and dies on the Color->uint conversion (dotcl-internal#357).
         (stripe (let ((b (dotnet:new "Avalonia.Media.SolidColorBrush")))
                   (dotnet:invoke b "set_Color" (dotnet:static "Avalonia.Media.Color" "Parse" "#808080"))
                   (dotnet:invoke b "set_Opacity" 0.09d0)
                   b)))
    (dotnet:invoke lb "set_MaxHeight" 220d0)
    (dotnet:static "Avalonia.Controls.Grid" "SetIsSharedSizeScope" lb t)
    (dotnet:invoke items "Add" (%deps-row "Library" "Version" "Author" :bold t))
    (loop for (name ver author) in deps
          for i from 0
          do (dotnet:invoke items "Add"
                            (%deps-row name ver author :stripe (and (oddp i) stripe))))
    lb))

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
          (when (%file-exists ico)
            (let ((img (dotnet:new "Avalonia.Controls.Image")))
              (dotnet:invoke img "set_Source" (dotnet:new "Avalonia.Media.Imaging.Bitmap" ico))
              (dotnet:invoke img "set_Width" 64d0)
              (dotnet:invoke img "set_Height" 64d0)
              (dotnet:invoke img "set_HorizontalAlignment"
                             (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Center"))
              (dotnet:invoke c "Add" img)))))
      (dotnet:invoke c "Add" (%about-text "paalam" t 20))
      (dotnet:invoke c "Add" (%about-text (format nil "Version ~a" *version*)))
      (dotnet:invoke c "Add" (%about-text *copyright*))
      (dotnet:invoke c "Add" (%about-text *repository-url*))
      (dotnet:invoke c "Add" (%about-text (format nil "License: ~a" *license*)))
      ;; Bundled libraries + versions, read live from the binaries (see %assembly-version),
      ;; as a striped two-column list.
      (handler-case
          (let ((deps (%about-deps)))
            (when deps
              (dotnet:invoke c "Add" (%about-text "Libraries" t 12))
              (dotnet:invoke c "Add" (%about-deps-list deps))))
        (error (e) (log-err "about-deps" e))))
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

;;; Nested books (#16): list the openable entries inside the current archive (nested archive / PDF)
;;; and open the chosen one. The book layer (book-subbooks) extracts the entry to a temp file; here
;;; we just present a picker and route the temp path through open-path (= the normal open flow).
(defun %pick-list (owner title items on-pick)
  "Modal single-select list. ITEMS = display strings. Double-click / Enter calls (on-pick index)
then closes; Esc cancels."
  (let* ((w    (dotnet:new "Avalonia.Controls.Window"))
         (root (dotnet:new "Avalonia.Controls.StackPanel"))
         (lb   (dotnet:new "Avalonia.Controls.ListBox")))
    (dotnet:invoke root "set_Margin" (dotnet:new "Avalonia.Thickness" 12d0))
    (dotnet:invoke root "set_Spacing" 8d0)
    (let ((coll (dotnet:invoke lb "get_Items")))
      (dolist (s items) (dotnet:invoke coll "Add" s)))
    (dotnet:invoke lb "set_SelectedIndex" 0)
    (dotnet:invoke lb "set_MinWidth" 360d0)
    (dotnet:invoke lb "set_MaxHeight" 360d0)
    (labels ((commit ()
               (let ((i (dotnet:invoke lb "get_SelectedIndex")))
                 (dotnet:invoke w "Close")
                 (when (>= i 0) (ignore-errors (funcall on-pick i))))))
      (dotnet:add-event lb "DoubleTapped" (lambda (s e) (declare (ignore s e)) (commit)))
      (dotnet:add-event w "KeyDown"
        (lambda (s e) (declare (ignore s))
          (let ((k (dotnet:invoke (dotnet:invoke e "get_Key") "ToString")))
            (cond ((or (string= k "Return") (string= k "Enter"))
                   (dotnet:invoke e "set_Handled" t) (commit))
                  ((string= k "Escape")
                   (dotnet:invoke e "set_Handled" t) (dotnet:invoke w "Close"))))))
      (dotnet:invoke (dotnet:invoke root "get_Children") "Add" lb)
      (dotnet:invoke w "set_Content" root)
      (dotnet:invoke w "set_Title" title)
      (dotnet:invoke w "set_CanResize" nil)
      (dotnet:invoke w "set_SizeToContent" (dotnet:static "Avalonia.Controls.SizeToContent" "WidthAndHeight"))
      (dotnet:invoke w "set_WindowStartupLocation"
                     (dotnet:static "Avalonia.Controls.WindowStartupLocation"
                                    (if owner "CenterOwner" "CenterScreen")))
      (if owner (dotnet:invoke w "ShowDialog" owner) (dotnet:invoke w "Show")))))

(defun %open-subbook (vw)
  "List nested openable books (#16) in the current book and open the chosen one."
  (let ((subs (book-subbooks (viewer-book vw))))
    (if (null subs)
        (%status vw "No nested books here")
        (%pick-list (viewer-window vw) "Open nested book"
                    (mapcar (lambda (s) (%basename (car s))) subs)
                    (lambda (i)
                      (multiple-value-bind (path tempp) (funcall (cdr (nth i subs)))
                        (when path
                          (open-path vw path)
                          (when tempp (setf (gethash (viewer-book vw) *book-temp-files*) path))
                          (%focus-viewport vw))))))))   ; restore key focus after the modal picker

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

