;;;; menu.lisp — paalam (#39 main.lisp split): menu bar (#21/#32)
(in-package :paalam)

;;; Menu bar (#21)
;;; Menu/MenuItem hold keyboard focus. If focus stays on the menu after a click,
;;; arrows/Space/Esc etc. get eaten by the menu navigation and never reach the
;;; viewer's KeyDown. So after a click we always return focus to the viewport.
(defun %focus-viewport (vw)
  (ignore-errors (dotnet:invoke (viewer-viewport vw) "Focus")))

(defun %menu-item (vw header &optional click)
  (let ((mi (dotnet:new "Avalonia.Controls.MenuItem")))
    (dotnet:invoke mi "set_Header" header)
    (when click
      (dotnet:add-event mi "Click"
        (lambda (s e) (declare (ignore s e)) (funcall click) (%focus-viewport vw))))
    mi))
(defun %menu-add (parent item) (dotnet:invoke (dotnet:invoke parent "get_Items") "Add" item))

(defun %key-label (k)
  "Tidy up an Avalonia Key name for menu display ([ ] 0 etc.)."
  (cond ((null k) nil)
        ((or (string= k "D0") (string= k "NumPad0")) "0")
        ((string= k "OemOpenBrackets") "[")
        ((string= k "OemCloseBrackets") "]")
        ((string= k "OemSemicolon") ":")
        ((string= k "OemComma") ",")
        ((string= k "OemPeriod") ".")
        (t k)))

(defun %menu-cmd (vw parent id)
  "Add command ID to PARENT as a menu item (showing label + representative key)."
  (let* ((c (%command id))
         (k (%key-label (%command-primary-key id)))
         (header (if k (format nil "~a (~a)" (command-label c) k) (command-label c))))
    (%menu-add parent (%menu-item vw header (lambda () (%run-command vw id))))))

;;; In-menu separator (#32). A separator is represented by a unique cons (:sep) (to avoid
;;; identifier collisions and to handle reorder/delete via eql identity). Round-trips as (:SEP) in menu.lisp.
(defun %make-menu-sep () (list :sep))
(defun %menu-sep-p (x) (and (consp x) (eq (car x) :sep)))
(defun %fresh-gkey (menu)
  "Return a new group keyword not present in MENU (a group list whose first of each element is the gkey)."
  (loop for i from 1
        for k = (intern (format nil "GROUP-~d" i) :keyword)
        unless (find k menu :key #'first) return k))
(defun %menu-render-entries (ids)
  "Tidy IDS (a mix of command ids and separators) for rendering: keep only existing commands and
separators, dropping leading/trailing/consecutive separators. Returns a sequence of command ids and :sep markers."
  (let ((out '()) (pending nil) (seen nil))
    (dolist (id ids)
      (cond ((%menu-sep-p id) (when seen (setf pending t)))
            ((%command id)
             (when pending (push :sep out) (setf pending nil))
             (push id out) (setf seen t))))
    (nreverse out)))

(defun %add-menu-groups (vw container)
  "Add group submenus to CONTAINER (Menu / ContextMenu) in *command-menu* order, and
arrange the relevant commands and separators in each group (shared by menu/context; #26/#32)."
  (let ((items (dotnet:invoke container "get_Items")))
    (dolist (g *command-menu*)
      (destructuring-bind (gkey title &rest ids) g
        (declare (ignore gkey))
        (let ((entries (%menu-render-entries ids)))
          (when entries        ; only show the group when it has at least one real command
            (let ((sub (%menu-item vw title)))
              (dolist (e entries)
                (if (eq e :sep)
                    (%menu-add sub (dotnet:new "Avalonia.Controls.Separator"))
                    (%menu-cmd vw sub e)))
              (dotnet:invoke items "Add" sub))))))))

(defun %build-menu (vw win)
  (declare (ignore win))   ; open-type commands use (viewer-window vw) as owner
  (let ((menu (dotnet:new "Avalonia.Controls.Menu")))
    (%add-menu-groups vw menu)
    menu))

(defun %build-context-menu (vw)
  "For viewport right-click. Built from the same *command-menu* definition as the menu (#26).
Right after a gesture fires (right-drag runs a command), Cancel Opening so it does not open (#29).
Because ContextMenu subscribes to ContextRequested itself on set_ContextMenu,
marking ContextRequested Handled from outside does not stop it -> stop it with Opening.Cancel."
  (let ((cm (dotnet:new "Avalonia.Controls.ContextMenu")))
    (%add-menu-groups vw cm)
    (dotnet:add-event cm "Opening"
                      (lambda (s e) (declare (ignore s))
                        (when (viewer-gesture-fired vw)
                          (dotnet:invoke e "set_Cancel" t)
                          (setf (viewer-gesture-fired vw) nil))))
    cm))

(defun %rebuild-menus (vw)
  "Rebuild the menu bar and context menu from *command-menu* (#26 apply edits immediately)."
  (when (viewer-menu vw)
    (dotnet:invoke (dotnet:invoke (viewer-menu vw) "get_Items") "Clear")
    (%add-menu-groups vw (viewer-menu vw)))
  (ignore-errors
    (dotnet:invoke (viewer-viewport vw) "set_ContextMenu" (%build-context-menu vw))))

;;; Menu editing (#26). Reorder/delete/add within a group -> persist to menu.lisp + apply immediately.
(defun %shift-in-list (lst item dir)
  "New list with ITEM in LST swapped with its neighbor in the dir (-1 up / +1 down) direction."
  (let ((v (copy-list lst)) (pos (position item lst)))
    (when pos
      (let ((j (+ pos dir)))
        (when (and (>= j 0) (< j (length v))) (rotatef (nth pos v) (nth j v)))))
    v))
(defun %insert-before (ids item before)
  "New list with ITEM inserted into IDS just before BEFORE (at the end if BEFORE is absent).
Assumes ITEM has already been removed by the caller (does not create duplicates)."
  (if (member before ids)
      (loop for x in ids
            when (eql x before) collect item
            collect x)
      (append ids (list item))))
(defun %menu-norm (menu)   ; (gkey title id...) → (gkey title (id...)) copy for editing
  (mapcar (lambda (g) (list (first g) (second g) (copy-list (cddr g)))) menu))
(defun %menu-denorm (menu) ; inverse transform
  (mapcar (lambda (g) (list* (first g) (second g) (third g))) menu))

;;; Mouse gesture editing (#29). Drag in the record box -> direction sequence, then pick a command to
;;; add/update. Saved immediately to gestures.lisp (*gestures* is referenced directly during a gesture).
