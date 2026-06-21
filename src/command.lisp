;;;; command.lisp — paalam (#39 main.lisp split): command table / keymap / menu config (#10)
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; Command table (#10): the single source of key bindings and menu.
;;; Each command has an id, a label (menu display), default keys (Avalonia
;;; Key.ToString names), and a handler (a function or symbol taking vw).
;;; Both key dispatch and menu construction are derived from here, so the
;;; shortcut notation is not maintained twice. The future key-binding edit
;;; UI (phase 2) edits this table and *keymap*.
;;; ------------------------------------------------------------------
(defstruct (command (:constructor make-command (id label keys handler)))
  id label keys handler)

;;; Command handlers (each takes a single vw).
(defun %next-spread (vw) (incf (viewer-sindex vw)) (show-spread vw))
(defun %prev-spread (vw) (decf (viewer-sindex vw)) (show-spread vw))
(defun %first-spread (vw) (setf (viewer-sindex vw) 0) (show-spread vw))
(defun %last-spread (vw)
  (setf (viewer-sindex vw) (1- (length (viewer-spreads vw)))) (show-spread vw))
(defun %reading-forward (vw)    ; <- : read forward along the binding direction
  (if (eq *page-direction* :rtl) (%next-spread vw) (%prev-spread vw)))
(defun %reading-backward (vw)   ; -> : read backward
  (if (eq *page-direction* :rtl) (%prev-spread vw) (%next-spread vw)))
(defun %toggle-fullscreen (vw) (%set-fullscreen vw (not (%fullscreen-p vw))))
(defun %escape (vw) (if (%fullscreen-p vw) (%set-fullscreen vw nil) (%quit vw)))
(defun %toggle-bookmark-current (vw)
  (toggle-bookmark (viewer-source vw) (%cur-page vw)) (%update-title vw))
(defun %clip-add-current (vw)
  "Add all pages of the current spread to the clip (a spread is both pages)."
  (let ((sp (viewer-spreads vw)))
    (when (plusp (length sp))
      (dolist (p (aref sp (viewer-sindex vw))) (clip-add (viewer-source vw) p))))
  (%clip-cache-warm)                   ; #35 ①: copy the added pages locally in the background
  (%update-title vw)
  (when (dotnet:invoke (viewer-clip-pane vw) "get_IsVisible") (%rebuild-clip-pane vw)))

(defparameter *commands*
  ;; Keys are not written in labels (the menu adds them from %keymap). keys are default bindings.
  (list
   (make-command :open-file     "Open File…"       '()        (lambda (vw) (%open-file-dialog vw (viewer-window vw))))
   (make-command :open-folder   "Open Folder…"     '()        (lambda (vw) (%open-folder-dialog vw (viewer-window vw))))
   (make-command :settings      "Settings…"        '()        (lambda (vw) (%open-settings vw)))
   (make-command :edit-menu     "Edit Menu…"       '()        '%open-menu-editor)
   (make-command :edit-gestures "Edit Mouse Gestures…" '()     '%open-gesture-editor)
   (make-command :quit          "Quit"             '("Q")     '%quit)
   (make-command :next-spread   "Next Spread"      '("PageDown" "Next" "Space" "J") '%next-spread)
   (make-command :prev-spread   "Prev Spread"      '("PageUp" "Prior" "Back" "K")   '%prev-spread)
   (make-command :reading-fwd   "Read Forward"     '("Left")  '%reading-forward)
   (make-command :reading-bwd   "Read Backward"    '("Right") '%reading-backward)
   (make-command :first-spread  "First Page"       '("Home")  '%first-spread)
   (make-command :last-spread   "Last Page"        '("End")   '%last-spread)
   (make-command :fullscreen    "Toggle Fullscreen" '("F11" "F") '%toggle-fullscreen)
   (make-command :spread        "Spread"           '("S")     '%toggle-spread)
   (make-command :direction     "Binding Direction" '("D")    '%toggle-direction)
   (make-command :recursive     "Recursive Scan"   '("R")     '%toggle-recursive)
   (make-command :fit           "Fit"              '("D0" "NumPad0") 'reset-view)
   (make-command :fit-mode      "Cycle Fit Mode"   '("W")     '%cycle-fit-mode)
   (make-command :rotate-right  "Rotate Right"     '("OemPeriod") '%rotate-right)
   (make-command :rotate-left   "Rotate Left"      '("OemComma")  '%rotate-left)
   (make-command :slideshow     "Slideshow"        '("F5")    '%slideshow-toggle)
   (make-command :loupe         "Loupe (Magnifier)" '("Z")    '%loupe-toggle)
   (make-command :goto          "Go to Page (gg=first / G=last / [count]g)" '("G") '%goto-page)
   (make-command :palette       "Command Palette"  '("OemSemicolon") '%command-palette)
   (make-command :search        "Search (/)"        '("OemQuestion" "Divide") '%command-search)
   (make-command :toc           "TOC (O)"           '("O")     '%command-toc)
   (make-command :clip-pane     "Clip Pane"        '("P")     '%toggle-clip-pane)
   (make-command :thumb-pane    "Shelf (Thumbnails)" '("T")   '%toggle-thumb-pane)
   (make-command :browse-pane   "Folder"           '("L")     '%toggle-browse-pane)
   (make-command :clip-add      "Add to Clip"      '("A")     '%clip-add-current)
   (make-command :export        "Export…"          '("E")     '%pane-export)
   (make-command :bookmark      "Toggle Bookmark"  '("B")     '%toggle-bookmark-current)
   (make-command :prev-bookmark "Prev Bookmark"    '("OemOpenBrackets")  (lambda (vw) (%goto-bookmark vw -1)))
   (make-command :next-bookmark "Next Bookmark"    '("OemCloseBrackets") (lambda (vw) (%goto-bookmark vw  1)))
   (make-command :escape        "Back / Quit"      '("Escape") '%escape)
   ;; About is reachable from the palette and the Help menu (Win/Linux) / app menu (macOS),
   ;; but is intentionally kept out of *command-menu* (not part of the user-editable menu).
   (make-command :about         "About paalam…"    '()        '%about))
  "All commands (in definition order). Menu/key-binding editing references this order.")

(defun %command (id) (find id *commands* :key #'command-id))

;;; Menu config (#26): the single source of the menu / context menu.
;;; An ordered list of (group-key  menu-title  command-id...). Edit here and
;;; the menu changes (eliminating the duplicate maintenance against a hand-written
;;; %build-menu). Unlisted commands (internal operations like reading-fwd/bwd,
;;; escape) are not shown in the menu.
(defparameter *command-menu*
  '((:file "File"  :open-file :open-folder :settings :edit-menu :edit-gestures :quit)
    (:view "View"  :fullscreen :fit :fit-mode :rotate-left :rotate-right :loupe :slideshow
                     :spread :direction :recursive
                     :browse-pane :thumb-pane :clip-pane)
    (:jump "Go"    :first-spread :prev-spread :next-spread :last-spread :goto
                     :prev-bookmark :next-bookmark)
    (:tool "Tools" :palette :search :toc)
    (:clip "Clip"  :clip-add :export :bookmark))
  "Menu config. Builds a submenu per group and lays out the matching commands.
Persisted to menu.lisp by user editing and swapped in (#26).")
(defparameter *default-command-menu* (copy-tree *command-menu*)
  "Shipped menu config (for the editor's \"restore defaults\").")

;;; Key bindings (#10 phase 2): user overrides held in *keybinds*; commands without
;;; one use the default keys. *keymap* (key name -> id) is built from that. Persisted
;;; to keymap.lisp (edited in the settings window).
(defparameter *keybinds* nil
  "User overrides ((command-id . (key ...)) ...). Commands without an entry use default keys.")
(defparameter *keymap* (make-hash-table :test 'equal)
  "Key name (Avalonia Key.ToString) -> command id. Built from *keybinds* + defaults.")

(defun %effective-keys (c)
  "Effective keys of command C (the override if any, otherwise the default)."
  (let ((o (assoc (command-id c) *keybinds*)))
    (if o (cdr o) (command-keys c))))

(defun rebuild-keymap ()
  "Rebuild *keymap* from *keybinds* + defaults."
  (clrhash *keymap*)
  (dolist (c *commands*)
    (dolist (k (%effective-keys c)) (setf (gethash k *keymap*) (command-id c)))))
(rebuild-keymap)

(defun load-keybinds ()
  (let ((d (%read-state "keymap.lisp"))) (when (listp d) (setf *keybinds* d)))
  (rebuild-keymap))
(defun save-keybinds () (%write-state "keymap.lisp" *keybinds*))

;;; Menu config persistence (#26). If menu.lisp exists, swap in *command-menu*.
(defun load-menu ()
  (let ((d (%read-state "menu.lisp")))
    (when (and (consp d) (consp (car d))) (setf *command-menu* d))))
(defun save-menu () (%write-state "menu.lisp" *command-menu*))

(defun %command-primary-key (id)
  "Representative key of ID (for menu display; prefers default-key order, else picks from keymap)."
  (let ((c (%command id)))
    (or (and c (find-if (lambda (k) (eq (gethash k *keymap*) id)) (command-keys c)))
        (loop for k being the hash-keys of *keymap* using (hash-value v)
              when (eq v id) return k))))

(defun %run-command (vw id)
  (let ((c (%command id)))
    (when c (funcall (command-handler c) vw))))

(defun %digit-key (kn)
  "If the key name (Avalonia Key.ToString) is a digit, return 0-9, else nil. D0-D9 / NumPad0-9."
  (cond ((and (= (length kn) 2) (char= (char kn 0) #\D)) (digit-char-p (char kn 1)))
        ((and (= (length kn) 7) (string= (subseq kn 0 6) "NumPad")) (digit-char-p (char kn 6)))
        (t nil)))

(defun %shift? (e)
  "Whether the KeyEventArgs carries the Shift modifier (for vi's g/G distinction; #23).
KeyModifiers gives the same Key.G for upper/lower case, so we distinguish by the modifier."
  (and (search "Shift"
               (dotnet:invoke (dotnet:invoke e "get_KeyModifiers") "ToString"))
       t))

(defun %ctrl? (e)
  "Whether the KeyEventArgs carries the Control modifier (e.g. Ctrl-n/p in the bar)."
  (and (search "Control"
               (dotnet:invoke (dotnet:invoke e "get_KeyModifiers") "ToString"))
       t))

(defun %on-key (vw e)
  "Window KeyDown. vi-style input (#23): digit=accumulate count, g/G=a small state machine
for first/last/go-to, otherwise look up *keymap* and run the command (the command reads count
via viewer-pending-count). A lone 0 does not start a count (keeps the existing 0=fit; only a 0
while a count is in progress shifts a digit).
g/G: [count]g=count-th page / gg=first / G(Shift)=last / lone g=wait for the next g.
While the command bar is shown, do nothing here (the TextBox handles input; prevents misfires)."
  (when (and (viewer-cmd-bar vw)
             (dotnet:invoke (viewer-cmd-bar vw) "get_IsVisible"))
    (return-from %on-key))
  (let* ((kn (dotnet:invoke (dotnet:invoke e "get_Key") "ToString"))
         (d  (%digit-key kn)))
    (cond
      ;; Arrow keys during the loupe move the loupe center (prevents turning into page turns; #28).
      ((and (viewer-loupe vw) (member kn '("Left" "Right" "Up" "Down") :test #'string=))
       (let ((step 60d0))
         (cond ((string= kn "Left")  (%loupe-nudge vw (- step) 0d0))
               ((string= kn "Right") (%loupe-nudge vw step 0d0))
               ((string= kn "Up")    (%loupe-nudge vw 0d0 (- step)))
               ((string= kn "Down")  (%loupe-nudge vw 0d0 step)))))
      ;; g / G — in Avalonia both cases are Key.G. Shift=last, count=go-to, twice=first.
      ((string= kn "G")
       (let ((cnt   (viewer-pending-count vw))
             (shift (%shift? e))
             (was-g (viewer-pending-g vw)))
         (cond (cnt   (%goto-page vw))      ; [count]g / [count]G -> count-th page
               (shift (%last-spread vw))    ; G -> last
               (was-g (%first-spread vw))   ; gg -> first
               (t     (setf (viewer-pending-g vw) t))) ; lone g -> wait for next key
         ;; Keep pending-g only in the "wait" branch (no count/shift/prev-g).
         (unless (and (not cnt) (not shift) (not was-g))
           (setf (viewer-pending-g vw) nil))
         (setf (viewer-pending-count vw) nil)))
      ;; n / N — cycle between search hits (only while searching; n=next, N=Shift=prev). Otherwise falls through to the normal keymap.
      ((and (string= kn "N") (viewer-search-matches vw))
       (%search-step vw (if (%shift? e) -1 1))
       (setf (viewer-pending-count vw) nil (viewer-pending-g vw) nil))
      ;; Digit: accumulate count
      ((and d (or (> d 0) (viewer-pending-count vw)))
       (setf (viewer-pending-g vw) nil
             (viewer-pending-count vw) (+ (* 10 (or (viewer-pending-count vw) 0)) d)))
      ;; Otherwise: normal command
      (t
       (setf (viewer-pending-g vw) nil)
       (let ((id (gethash kn *keymap*)))
         (when id (%run-command vw id))     ; count is read via pending-count during execution
         (setf (viewer-pending-count vw) nil))))))

