;;;; config-io.lisp — paalam (#39 main.lisp split): settings import/export (#24) + settings window
(in-package :paalam)

;;; Settings import / export (#24): write/read the whole settings set as 1 file (s-expr plist).
;;; Targets: settings / keybinds(keymap) / gestures / menu / bookmarks. Unknown keys are ignored,
;;; missing sections keep current. clip/history are path-dependent and volatile, so not included.
(defun %config-bundle ()
  "Gather the whole current settings set into one plist (for export). Leading :paalam-config is the version."
  (list :paalam-config 1
        :settings (loop for e in *settings*
                        append (list (car e) (symbol-value (cdr e))))
        :keybinds *keybinds*
        :gestures *gestures*
        :menu     *command-menu*
        :bookmarks *bookmarks*))

(defun %apply-config-bundle (vw bundle)
  "Apply and persist the imported BUNDLE. Unknown keys are ignored, missing sections keep current."
  (when (and (consp bundle) (getf bundle :paalam-config))
    (let ((s (getf bundle :settings)))
      (when (consp s)
        (loop for (k v) on s by #'cddr
              for e = (assoc k *settings*)
              when e do (setf (symbol-value (cdr e)) v))
        (save-settings)))
    (let ((kb (getf bundle :keybinds)))
      (when kb (setf *keybinds* kb) (rebuild-keymap) (save-keybinds)))
    (let ((g (getf bundle :gestures)))
      (when g (setf *gestures* g) (save-gestures)))
    (let ((m (getf bundle :menu)))
      (when m (setf *command-menu* m) (save-menu) (%rebuild-menus vw)))
    (let ((b (getf bundle :bookmarks)))
      (when b (setf *bookmarks* b) (save-bookmarks)))
    ;; Apply to viewer: theme/background/interpolation + reopen the book to rebuild caches etc.
    (%apply-theme)
    (%reopen vw)
    (%apply-background vw)
    (%apply-interpolation vw)
    t))

(defun %export-config (vw)
  (dotnet:static "Paalam.DialogHelper" "PickSaveConfig" (viewer-window vw) "paalam-config.lisp"
                 (dotnet:make-delegate "System.Action`1[System.String]"
                   (lambda (path)
                     (%atomic-write path (%config-bundle))
                     (%ui-post (lambda () (%status vw (format nil "Exported settings → ~a" path))))))))

(defun %import-config (vw &optional after)
  "Pick a settings file and apply it. If AFTER is given, call it after applying (e.g. close the settings window)."
  (dotnet:static "Paalam.DialogHelper" "PickOpenConfig" (viewer-window vw)
                 (dotnet:make-delegate "System.Action`1[System.String]"
                   (lambda (path)
                     (let ((bundle (%read-sexp-file path)))
                       (%ui-post
                        (lambda ()
                          (cond ((and (consp bundle) (getf bundle :paalam-config))
                                 (%apply-config-bundle vw bundle)
                                 (%status vw "Imported settings")
                                 (when after (funcall after)))
                                (t (%status vw "Not a paalam settings file"))))))))))

(defun %open-settings (vw)
  ;; Left: collapsible tree (categories), right: the matching page. Selection swaps the right side (master/detail).
  ;; OK/Cancel are shared across all pages (controls keep their values even when hidden, so OK can read them).
  (let* ((owner (viewer-window vw))
         (swin  (dotnet:new "Avalonia.Controls.Window"))
         (dir-img-combo (%s-choice-combo *dir-choices* *image-direction*))   ; binding direction for image/archive (#33)
         (dir-pdf-combo (%s-choice-combo *dir-choices* *pdf-direction*))     ; binding direction for PDF (#33)
         (cb-spread (%s-checkbox *spread*))
         (cb-fps    (%s-checkbox *first-page-single*))
         (cb-rec    (%s-checkbox *recursive*))
         (tb-mem    (%s-textbox *cache-memory-mb*))
         (tb-cache  (%s-textbox *cache-size*))
         (tb-pf     (%s-textbox *prefetch-ahead*))
         (tb-th     (%s-textbox *loader-threads*))
         (tb-slide  (%s-textbox *slideshow-interval*))
         (fit-combo    (%s-choice-combo *fit-choices* *fit-mode*))
         (bg-combo     (%s-choice-combo *bg-choices* *background-color* #'string=))
         (interp-combo (%s-choice-combo *interp-choices* *interpolation*))
         (theme-combo  (%s-choice-combo *theme-choices* *theme*))
         (exp-w-combo  (%s-choice-combo *export-width-choices* *export-width*))   ; max export width (#35 ②)
         (tb-dpi    (%s-textbox *pdf-dpi*))
         ;; Working copy of key bindings. (car kbox) is the working list. From closures,
         ;; overwrite the place with (setf (car kbox) ..) (rebinding a let* var produces invalid IL in dotcl).
         (kbox (list (let ((acc '()))
                       (dolist (c *commands*)
                         (dolist (k (%effective-keys c)) (push (cons k (command-id c)) acc)))
                       (nreverse acc))))
         (kb-stack (dotnet:new "Avalonia.Controls.StackPanel"))
         (content  (dotnet:new "Avalonia.Controls.ContentControl"))
         (win-p (dotnet:static "System.OperatingSystem" "IsWindows"))  ; file association is Windows only (#11)
         (assoc-status (dotnet:new "Avalonia.Controls.TextBlock"))
         (cur (list "Layout"))   ; current page name (fallback when clearing search; #27)
         ;; Search index (item label . page name). Page builder (spage) registers each row, so
         ;; there is no double bookkeeping. For pages that don't use %s-row because of their special
         ;; kind (keybinds/file association), only aliases are seeded here (page names themselves are
         ;; looked up by do-search directly from pages).
         (sidx (list (list (cons "Shortcut" "Keybinds") (cons "Key binding" "Keybinds")
                           (cons "Extension" "File Association") (cons "File association" "File Association")))))
    (labels
        ((add-binding (k)
           ;; Digits 1-9 are reserved for the count prefix, so binding them won't fire — reject them (#23). 0 is OK.
           (let ((d (%digit-key k)))
             (unless (or (and d (> d 0)) (assoc k (car kbox) :test #'string=))
               (setf (car kbox) (cons (cons k (command-id (car *commands*))) (car kbox)))))
           (refresh))
         (remove-binding (pair) (setf (car kbox) (remove pair (car kbox))) (refresh))
         (kb-row (pair)
           (let ((row  (dotnet:new "Avalonia.Controls.DockPanel"))
                 (klbl (dotnet:new "Avalonia.Controls.TextBlock"))
                 (cbox (dotnet:new "Avalonia.Controls.ComboBox"))
                 (bdel (dotnet:new "Avalonia.Controls.Button")))
             (dotnet:invoke klbl "set_Text" (%key-label (car pair)))
             (dotnet:invoke klbl "set_Width" 90d0)
             (dotnet:invoke klbl "set_VerticalAlignment"
                            (dotnet:static "Avalonia.Layout.VerticalAlignment" "Center"))
             (let ((items (dotnet:invoke cbox "get_Items")))
               (dolist (c *commands*) (dotnet:invoke items "Add" (command-label c))))
             (dotnet:invoke cbox "set_SelectedIndex"
                            (or (position (cdr pair) *commands* :key #'command-id) 0))
             (dotnet:add-event cbox "SelectionChanged"
               (lambda (s e) (declare (ignore s e))
                 (let ((i (dotnet:invoke cbox "get_SelectedIndex")))
                   (when (>= i 0) (setf (cdr pair) (command-id (nth i *commands*)))))))
             (dotnet:invoke bdel "set_Content" "×")
             (dotnet:invoke bdel "set_Margin" (dotnet:new "Avalonia.Thickness" 6d0 0d0 0d0 0d0))
             (dotnet:add-event bdel "Click"
               (lambda (s e) (declare (ignore s e)) (remove-binding pair)))
             (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" klbl
                            (dotnet:static "Avalonia.Controls.Dock" "Left"))
             (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" bdel
                            (dotnet:static "Avalonia.Controls.Dock" "Right"))
             (dotnet:invoke row "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 2d0))
             (let ((rc (dotnet:invoke row "get_Children")))
               (dotnet:invoke rc "Add" klbl)
               (dotnet:invoke rc "Add" bdel)
               (dotnet:invoke rc "Add" cbox))
             row))
         (refresh ()
           (let ((ch (dotnet:invoke kb-stack "get_Children")))
             (dotnet:invoke ch "Clear")
             (dolist (pair (sort (copy-list (car kbox)) #'string< :key #'car))
               (dotnet:invoke ch "Add" (kb-row pair)))))
         (page (rows)
           "A settings page (StackPanel) stacking rows vertically."
           (let ((p (dotnet:new "Avalonia.Controls.StackPanel")))
             (dotnet:invoke p "set_Margin" (dotnet:new "Avalonia.Thickness" 16d0))
             (let ((ch (dotnet:invoke p "get_Children")))
               (dolist (r rows) (dotnet:invoke ch "Add" r)))
             p))
         (sd (key) (cdr (assoc key *setting-defaults*)))
         (reset-check (cb key) (lambda () (%set-check cb (sd key))))
         (reset-combo (combo choices key &optional (test #'eql))
           (lambda () (dotnet:invoke combo "set_SelectedIndex"
                                     (or (position (sd key) choices :key #'cdr :test test) 0))))
         (reset-text (tb key) (lambda () (dotnet:invoke tb "set_Text" (format nil "~a" (sd key)))))
         (spage (name rows)
           "A settings page with a NAME header + rows of (label control default-thunk). Each label is
registered in the search index sidx (page definitions are the source for search; #27), with a trailing
\"Reset this page to default\" button (calls each thunk to set controls to factory values; globals are
left untouched until OK; #27)."
           (let ((resets (mapcar #'third rows)))
             (page (append
                    (cons (%s-header name)
                          (mapcar (lambda (r)
                                    (push (cons (first r) name) (car sidx))
                                    (%s-row (first r) (second r)))
                                  rows))
                    (list (%s-button "Reset this page to default"
                                     (lambda () (dolist (f resets) (funcall f)))))))))
         (tvi (header &optional children)
           "Make a TreeViewItem (an expanded parent node if children are given)."
           (let ((it (dotnet:new "Avalonia.Controls.TreeViewItem")))
             (dotnet:invoke it "set_Header" header)
             (when children
               (dotnet:invoke it "set_IsExpanded" t)
               (let ((items (dotnet:invoke it "get_Items")))
                 (dolist (c children) (dotnet:invoke items "Add" c))))
             it))
         (assoc-page ()
           "File association page (#11): a checkbox per extension + register/unregister (Windows)."
           (let ((wrap  (dotnet:new "Avalonia.Controls.WrapPanel"))
                 (boxes (mapcar (lambda (ext)
                                  (let ((cb (dotnet:new "Avalonia.Controls.CheckBox")))
                                    (dotnet:invoke cb "set_Content" (format nil ".~a" ext))
                                    (dotnet:invoke cb "set_Width" 92d0)
                                    (dotnet:invoke cb "set_IsThreeState" nil)
                                    (%set-check cb (and win-p (%assoc-registered-p ext)))
                                    (cons ext cb)))
                                *assoc-extensions*)))
             (dotnet:invoke wrap "set_Width" 380d0)
             (dolist (b boxes) (dotnet:invoke (dotnet:invoke wrap "get_Children") "Add" (cdr b)))
             (labels ((checked-exts ()
                        (let ((acc '()))
                          (dolist (b boxes) (when (dotnet:invoke (cdr b) "get_IsChecked") (push (car b) acc)))
                          (nreverse acc)))
                      (set-all (v) (dolist (b boxes) (%set-check (cdr b) v)))
                      (sync ()  ; reflect the actual registry state into the checkboxes
                        (dolist (b boxes)
                          (%set-check (cdr b) (%assoc-registered-p (car b))))))
               (let ((row (dotnet:new "Avalonia.Controls.StackPanel")))
                 (dotnet:invoke row "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
                 (let ((rc (dotnet:invoke row "get_Children")))
                   (dotnet:invoke rc "Add" (%s-button "Select all" (lambda () (set-all t))))
                   (dotnet:invoke rc "Add" (%s-button "Clear all" (lambda () (set-all nil))))
                   (dotnet:invoke rc "Add"
                     (%s-button "Register"
                       (lambda ()
                         (let ((es (checked-exts)))
                           (handler-case
                               (progn (%assoc-register es) (sync)
                                      (dotnet:invoke assoc-status "set_Text"
                                        (format nil "Registered ~d type(s) as \"Open with\" candidates." (length es))))
                             (error (e) (dotnet:invoke assoc-status "set_Text" (format nil "Failed: ~a" e))))))))
                   (dotnet:invoke rc "Add"
                     (%s-button "Unregister"
                       (lambda ()
                         (handler-case
                             (progn (%assoc-unregister (checked-exts)) (sync)
                                    (dotnet:invoke assoc-status "set_Text" "Unregistered the selected associations."))
                           (error (e) (dotnet:invoke assoc-status "set_Text" (format nil "Failed: ~a" e))))))))
                 (let ((desc (dotnet:new "Avalonia.Controls.TextBlock")))
                   (dotnet:invoke desc "set_Text"
                     "Pick extensions to associate and \"Register\" (per-user, no admin rights). This only adds them as candidates and doesn't change the default, so to make a type default, choose paalam in 'Default apps'.")
                   (dotnet:invoke desc "set_TextWrapping" (dotnet:static "Avalonia.Media.TextWrapping" "Wrap"))
                   (dotnet:invoke desc "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 0d0 8d0))
                   ;; Setting as default is optional. Registering doesn't auto-open it; only those who want to press it go to Windows settings.
                   (let ((defbtn (%s-button "Open Default Apps settings…" (lambda () (%open-default-apps)))))
                     (dotnet:invoke defbtn "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 8d0 0d0 0d0))
                     (dotnet:invoke defbtn "set_HorizontalAlignment"
                                    (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Left"))
                     (page (list (%s-header "File Association (Windows / no admin rights)")
                                 desc wrap row defbtn assoc-status)))))))))
      (refresh)
      (let* ((scr (dotnet:new "Avalonia.Controls.ScrollViewer"))
             (addbtn (%s-button "+ Add key"
                                (lambda () (%capture-key swin (lambda (k) (add-binding k))))))
             ;; Reset to default (#23): rebuild the working list from command-keys (factory defaults).
             ;; Saving with OK becomes a snapshot of the current defaults (= a way to pull in new defaults).
             (reset-btn (%s-button "Reset to default"
                          (lambda ()
                            (setf (car kbox)
                                  (let ((acc '()))
                                    (dolist (c *commands*)
                                      (dolist (k (command-keys c))
                                        (push (cons k (command-id c)) acc)))
                                    (nreverse acc)))
                            (refresh))))
             (panel-layout
               (spage "Layout"
                      (list (list "Binding direction (image/archive)" dir-img-combo (reset-combo dir-img-combo *dir-choices* :image-direction))
                            (list "Binding direction (PDF)" dir-pdf-combo (reset-combo dir-pdf-combo *dir-choices* :pdf-direction))
                            (list "Spread" cb-spread (reset-check cb-spread :spread))
                            (list "Show cover alone" cb-fps (reset-check cb-fps :first-page-single))
                            (list "Recursively collect subfolders" cb-rec (reset-check cb-rec :recursive))
                            (list "Fit" fit-combo (reset-combo fit-combo *fit-choices* :fit-mode)))))
             (panel-quality
               (spage "Image Quality"
                      (list (list "Theme" theme-combo (reset-combo theme-combo *theme-choices* :theme))
                            (list "Background color" bg-combo (reset-combo bg-combo *bg-choices* :background-color #'string=))
                            (list "Zoom interpolation" interp-combo (reset-combo interp-combo *interp-choices* :interpolation))
                            (list "PDF resolution (DPI)" tb-dpi (reset-text tb-dpi :pdf-dpi))
                            (list "Max export width" exp-w-combo (reset-combo exp-w-combo *export-width-choices* :export-width)))))
             (panel-perf
               (spage "Prefetch"
                      (list (list "Cache limit (MB)" tb-mem (reset-text tb-mem :cache-memory-mb))
                            (list "Max cached pages" tb-cache (reset-text tb-cache :cache-size))
                            (list "Prefetch spread count" tb-pf (reset-text tb-pf :prefetch-ahead))
                            (list "Loader threads" tb-th (reset-text tb-th :loader-threads))
                            (list "Slideshow interval (sec)" tb-slide (reset-text tb-slide :slideshow-interval)))))
             (panel-keys
               (progn
                 (dotnet:invoke scr "set_MaxHeight" 360d0)
                 (dotnet:invoke scr "set_Content" kb-stack)
                 (page (list (%s-header "Keybinds (key → command. one command per key, multiple allowed. digits 1-9 reserved for count)")
                             scr addbtn reset-btn))))
             (panel-assoc (assoc-page))
             (panel-io
               (let ((row (dotnet:new "Avalonia.Controls.StackPanel"))
                     (desc (dotnet:new "Avalonia.Controls.TextBlock")))
                 (dotnet:invoke row "set_Orientation"
                                (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
                 (let ((rc (dotnet:invoke row "get_Children")))
                   (dotnet:invoke rc "Add" (%s-button "Export…" (lambda () (%export-config vw))))
                   (dotnet:invoke rc "Add"
                     (%s-button "Import…"
                                (lambda () (%import-config vw (lambda () (dotnet:invoke swin "Close")))))))
                 (dotnet:invoke desc "set_Text"
                   "Write / read all settings (layout, image quality, prefetch settings / keybinds / menu layout / mouse gestures / bookmarks) to/from a single file. For carrying between environments, sharing, and backup. Import applies immediately and closes this settings window (unknown items ignored, missing items keep current).")
                 (dotnet:invoke desc "set_TextWrapping" (dotnet:static "Avalonia.Media.TextWrapping" "Wrap"))
                 (dotnet:invoke desc "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 0d0 10d0))
                 (page (list (%s-header "Settings I/O") desc row))))
             (pages (append (list (cons "Layout"        panel-layout)
                                  (cons "Image Quality" panel-quality)
                                  (cons "Prefetch"      panel-perf)
                                  (cons "Keybinds"      panel-keys)
                                  (cons "Settings I/O"  panel-io))
                            (when win-p (list (cons "File Association" panel-assoc)))))
             (tree (dotnet:new "Avalonia.Controls.TreeView"))
             (leaf-layout  (tvi "Layout"))
             (leaf-quality (tvi "Image Quality"))
             (leaf-perf    (tvi "Prefetch"))
             (leaf-keys    (tvi "Keybinds"))
             (leaf-io      (tvi "Settings I/O"))
             (leaf-assoc   (tvi "File Association"))
             (g-view (tvi "Display" (list leaf-layout leaf-quality)))
             (g-perf (tvi "Performance" (list leaf-perf)))
             (g-op   (tvi "Controls" (list leaf-keys)))
             (g-system (tvi "System" (if win-p (list leaf-io leaf-assoc) (list leaf-io))))
             (leaves (append (list (cons "Layout" leaf-layout) (cons "Image Quality" leaf-quality)
                                   (cons "Prefetch" leaf-perf) (cons "Keybinds" leaf-keys)
                                   (cons "Settings I/O" leaf-io))
                             (when win-p (list (cons "File Association" leaf-assoc)))))
             (sbox (dotnet:new "Avalonia.Controls.TextBox"))   ; settings search box (#27)
             ;; Navigate to the page for name (via tree selection → SelectionChanged swaps content).
             (nav (lambda (name)
                    (dotnet:invoke sbox "set_Text" "")        ; clear the search, then
                    (let ((lf (cdr (assoc name leaves :test #'equal))))
                      (when lf (dotnet:invoke lf "set_IsSelected" t)))))
             ;; Search: if empty show the current page; if non-empty show a list of hit (page › item) buttons on the right.
             (do-search
               (lambda (q)
                 (let ((qq (string-downcase (string-trim '(#\Space #\Tab) (or q "")))))
                   (if (zerop (length qq))
                       (dotnet:invoke content "set_Content"
                                      (cdr (assoc (car cur) pages :test #'equal)))
                       (let ((res (dotnet:new "Avalonia.Controls.StackPanel"))
                             (hits (remove-duplicates
                                    (append
                                     ;; Page names themselves (Layout/Image Quality/…) are also hit targets.
                                     (loop for (nm . pg) in pages
                                           when (search qq (string-downcase nm))
                                           collect (cons nm nm))
                                     ;; Item labels registered by each page + aliases of special pages.
                                     (loop for (label . page) in (car sidx)
                                           when (search qq (string-downcase label))
                                           collect (cons label page)))
                                    :test #'equal :from-end t)))
                         (dotnet:invoke res "set_Margin" (dotnet:new "Avalonia.Thickness" 16d0))
                         (if hits
                             (dolist (h hits)
                               (let* ((label (car h)) (page (cdr h))
                                      (btn (%s-button (format nil "~a > ~a" page label)
                                                      (let ((pg page)) (lambda () (funcall nav pg))))))
                                 (dotnet:invoke btn "set_HorizontalAlignment"
                                                (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Left"))
                                 (dotnet:invoke btn "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 2d0))
                                 (dotnet:invoke (dotnet:invoke res "get_Children") "Add" btn)))
                             (let ((tb (dotnet:new "Avalonia.Controls.TextBlock")))
                               (dotnet:invoke tb "set_Text" "No matches")
                               (dotnet:invoke (dotnet:invoke res "get_Children") "Add" tb)))
                         (dotnet:invoke content "set_Content" res))))))
             (root (dotnet:new "Avalonia.Controls.DockPanel"))
             (grid (dotnet:new "Avalonia.Controls.Grid"))
             (btns (dotnet:new "Avalonia.Controls.StackPanel")))
        ;; Left tree (collapsible categories). Look up the page from the selected leaf's header and show it on the right.
        (let ((items (dotnet:invoke tree "get_Items")))
          (dotnet:invoke items "Add" g-view)
          (dotnet:invoke items "Add" g-perf)
          (dotnet:invoke items "Add" g-op)
          (dotnet:invoke items "Add" g-system))   ; System: Settings I/O (+ File Association is Windows only)
        (dotnet:add-event tree "SelectionChanged"
          (lambda (s e) (declare (ignore s e))
            (let ((item (dotnet:invoke tree "get_SelectedItem")))
              (when item
                (let* ((name (dotnet:invoke item "get_Header"))
                       (p (cdr (assoc name pages :test #'equal))))
                  (when p (setf (car cur) name) (dotnet:invoke content "set_Content" p)))))))
        ;; Search box: typing shows cross-cutting hits as page › item on the right (#27).
        (dotnet:invoke sbox "set_Watermark" "Search settings…")
        (dotnet:invoke sbox "set_Margin" (dotnet:new "Avalonia.Thickness" 12d0 12d0 12d0 0d0))
        (dotnet:add-event sbox "TextChanged"
          (lambda (s e) (declare (ignore s e))
            (funcall do-search (dotnet:invoke sbox "get_Text"))))
        (dotnet:invoke content "set_Content" panel-layout)   ; initial page
        (dotnet:invoke leaf-layout "set_IsSelected" t)
        ;; 2-column grid: [tree(180) | content(*)]
        (let ((cols (dotnet:invoke grid "get_ColumnDefinitions"))
              (c0 (dotnet:new "Avalonia.Controls.ColumnDefinition")))
          (dotnet:invoke c0 "set_Width" (dotnet:new "Avalonia.Controls.GridLength" 180d0))
          (dotnet:invoke cols "Add" c0)
          (dotnet:invoke cols "Add" (dotnet:new "Avalonia.Controls.ColumnDefinition")))
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" tree 0)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" content 1)
        (let ((gc (dotnet:invoke grid "get_Children")))
          (dotnet:invoke gc "Add" tree)
          (dotnet:invoke gc "Add" content))
        ;; OK/Cancel (shared at the bottom)
        (dotnet:invoke btns "set_Orientation"
                       (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
        (dotnet:invoke btns "set_HorizontalAlignment"
                       (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Right"))
        (dotnet:invoke btns "set_Margin" (dotnet:new "Avalonia.Thickness" 12d0))
        (let ((bc (dotnet:invoke btns "get_Children")))
          (dotnet:invoke bc "Add" (%s-button "Cancel" (lambda () (dotnet:invoke swin "Close"))))
          (dotnet:invoke bc "Add"
            (%s-button "OK"
              (lambda ()
                (%settings-apply vw cb-spread cb-fps cb-rec tb-cache tb-pf tb-th
                                 fit-combo bg-combo interp-combo tb-dpi tb-mem tb-slide
                                 dir-img-combo dir-pdf-combo theme-combo exp-w-combo)
                ;; Regroup the key-oriented working list by command and commit (deletions reflected as empty lists).
                (setf *keybinds*
                      (mapcar (lambda (c)
                                (let ((ks '()))
                                  (dolist (pair (car kbox))
                                    (when (eq (cdr pair) (command-id c)) (push (car pair) ks)))
                                  (cons (command-id c) (nreverse ks))))
                              *commands*))
                (rebuild-keymap)
                (save-keybinds)
                (dotnet:invoke swin "Close")))))
        ;; root: [sbox(top) | btns(bottom) | grid(rest)]
        (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" sbox
                       (dotnet:static "Avalonia.Controls.Dock" "Top"))
        (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" btns
                       (dotnet:static "Avalonia.Controls.Dock" "Bottom"))
        (let ((rc (dotnet:invoke root "get_Children")))
          (dotnet:invoke rc "Add" sbox)
          (dotnet:invoke rc "Add" btns)
          (dotnet:invoke rc "Add" grid))
        (dotnet:invoke swin "set_Title" "Settings")
        (dotnet:invoke swin "set_Width" 640d0)
        (dotnet:invoke swin "set_Height" 520d0)
        (dotnet:invoke swin "set_WindowStartupLocation"
                       (dotnet:static "Avalonia.Controls.WindowStartupLocation" "CenterOwner"))
        (dotnet:invoke swin "set_Content" root)
        (dotnet:invoke swin "ShowDialog" owner)))))

