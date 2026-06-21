;;;; palette.lisp — paalam (#39 main.lisp split): command palette/search/TOC/cmdbar/status (#23)
(in-package :paalam)

;;; : command palette (#23). Filter *commands* by name/label and run.
(defun %palette-name (c)
  "Identifier typed in the palette (id lowercased; e.g. :next-spread → \"next-spread\")."
  (string-downcase (symbol-name (command-id c))))

(defun %palette-matches (query)
  "Return commands matching QUERY in rank order (exact → prefix → substring → label match).
Empty returns all of *commands*. Within the same rank, keep the original order of *commands*."
  (let ((q (string-downcase (string-trim ": " query))))   ; ignore leading : / whitespace
    (if (string= q "")
        (copy-list *commands*)
        (let ((scored '()))
          (dolist (c *commands*)
            (let* ((name (%palette-name c))
                   (rank (cond ((string= name q) 0)
                               ((eql 0 (search q name)) 1)
                               ((search q name) 2)
                               ((search q (command-label c)) 3)
                               (t nil))))
              (when rank (push (cons rank c) scored))))
          (mapcar #'cdr (stable-sort (nreverse scored) #'< :key #'car))))))

;;; / full-text search (#23). pdf uses body text (PdfText/FPDFText), others use page name. Cycle between hits with n/N.
(defun %array->list (arr)
  (when arr
    (loop for i from 0 below (dotnet:invoke arr "get_Length")
          collect (dotnet:invoke arr "GetValue" i))))

(defgeneric book-search (book query)
  (:documentation "List of 0-based page indices containing QUERY (in order of occurrence)."))
(defmethod book-search ((b pdf-book) query)
  (%array->list (dotnet:invoke (pdfb-doc b) "SearchPages" query)))
(defmethod book-search ((b t) query)
  ;; For non-pdf, substring match on page name (file name) (case-insensitive).
  (let ((q (string-downcase query)) (acc '()))
    (dotimes (i (book-page-count b))
      (when (search q (string-downcase (or (book-page-name b i) "")))
        (push i acc)))
    (nreverse acc)))

;;; table of contents (TOC) (#23). Turn pdf bookmarks into a (depth page title) list. Other formats have none.
(defun %parse-outline (s)
  "Turn the \"depth<TAB>page<TAB>title\" line string from PdfText.Outline into a (depth page title) list."
  (when (and s (plusp (length s)))
    (let ((entries '()))
      (with-input-from-string (in s)
        (loop for line = (read-line in nil nil) while line do
          (let* ((t1 (position #\Tab line))
                 (t2 (and t1 (position #\Tab line :start (1+ t1)))))
            (when t2
              (let ((depth (ignore-errors (parse-integer line :end t1)))
                    (page  (ignore-errors (parse-integer line :start (1+ t1) :end t2)))
                    (title (subseq line (1+ t2))))
                (when (and depth page)
                  (push (list depth page title) entries)))))))
      (nreverse entries))))

(defgeneric book-outline (book)
  (:documentation "List of TOC entries (depth page title). nil if none."))
(defmethod book-outline ((b pdf-book)) (%parse-outline (dotnet:invoke (pdfb-doc b) "Outline")))
(defmethod book-outline ((b t)) nil)

(defun %run-search (vw query)
  "Search the current book with QUERY. If there are matches, jump to the first one (count shown in title)."
  (let* ((book (viewer-book vw))
         ;; The / (or :) used to open the bar slips in at the front, so strip it (also matches vi's /word habit).
         (q (string-trim '(#\Space #\/ #\:) (or query "")))
         (ms (and book (plusp (length q)) (ignore-errors (book-search book q)))))
    (setf (viewer-search-matches vw) ms (viewer-search-index vw) 0 (viewer-search-query vw) q)
    (if ms
        (progn (%jump-to-page vw (first ms))   ; jump→render clears status→re-shown below
               (%highlight-match vw)
               (%status vw (format nil "search 1/~d hits  n/N to move" (length ms))))
        (when (plusp (length q))
          (%status vw (format nil "\"~a\" not found" q))))))

(defun %search-step (vw dir)
  "Cycle through search hits in the dir (+1/-1) direction and jump (n/N)."
  (let ((ms (viewer-search-matches vw)))
    (when ms
      (setf (viewer-search-index vw) (mod (+ (viewer-search-index vw) dir) (length ms)))
      (%jump-to-page vw (nth (viewer-search-index vw) ms))
      (%highlight-match vw)
      (%status vw (format nil "search ~d/~d hits  n/N to move" (1+ (viewer-search-index vw)) (length ms))))))

(defun %clear-highlights (vw)
  "Remove the overlaid search highlight rectangles from the stage canvas."
  (let ((hl (viewer-search-hl vw)))
    (when hl
      (let ((canvas (ignore-errors (dotnet:invoke (viewer-left-img vw) "get_Parent"))))
        (when canvas
          (let ((children (dotnet:invoke canvas "get_Children")))
            (dolist (r hl) (ignore-errors (dotnet:invoke children "Remove" r))))))
      (setf (viewer-search-hl vw) nil))))

(defun %highlight-match (vw)
  "Expand the (normalized) match rectangles of the current hit page to the page display size and overlay them on the stage in translucent yellow.
pdf-book only (other formats with page-name search have no rectangles)."
  (%clear-highlights vw)
  (let ((ms (viewer-search-matches vw))
        (book (viewer-book vw))
        (q (viewer-search-query vw)))
    (when (and ms q (typep book 'pdf-book))
      (let* ((page (nth (viewer-search-index vw) ms))
             (lay (assoc page (viewer-page-offsets vw)))
             (canvas (ignore-errors (dotnet:invoke (viewer-left-img vw) "get_Parent"))))
        (when (and lay canvas)
          (destructuring-bind (xoff yoff w h) (cdr lay)
            (let ((coords (%array->list (dotnet:invoke (pdfb-doc book) "MatchRects" page q)))
                  (children (dotnet:invoke canvas "get_Children")))
              (loop for (rx ry rw rh) on coords by #'cddddr while rh do
                (let ((rect (dotnet:new "Avalonia.Controls.Shapes.Rectangle")))
                  (dotnet:invoke rect "set_Width"  (coerce (* rw w) 'double-float))
                  (dotnet:invoke rect "set_Height" (coerce (* rh h) 'double-float))
                  (dotnet:invoke rect "set_Fill" (dotnet:static "Avalonia.Media.Brushes" "Yellow"))
                  (dotnet:invoke rect "set_Opacity" 0.5d0)
                  (dotnet:invoke rect "set_IsHitTestVisible" nil)
                  (dotnet:static "Avalonia.Controls.Canvas" "SetLeft" rect (coerce (+ xoff (* rx w)) 'double-float))
                  (dotnet:static "Avalonia.Controls.Canvas" "SetTop"  rect (coerce (+ yoff (* ry h)) 'double-float))
                  (dotnet:invoke children "Add" rect)
                  (push rect (viewer-search-hl vw)))))))))))

(defun %palette-row (c)
  "One candidate row: \"label  name  [key]\" (so the name to type is clear)."
  (let ((k (%key-label (%command-primary-key (command-id c)))))
    (format nil "~a   ~a~@[   [~a]~]" (command-label c) (%palette-name c) k)))

(defun %cmd-populate (vw)
  "Update the candidate list according to input (select the first)."
  (let* ((tb  (viewer-cmd-input vw))
         (lb  (viewer-cmd-list vw))
         (raw (or (dotnet:invoke tb "get_Text") "")))
    ;; Right after opening with :, that : may slip into the field. Strip the leading : to
    ;; tidy display and search (also matches vi's :q habit). After stripping there is no :, so re-TextChanged does not recurse.
    (when (and (plusp (length raw)) (char= (char raw 0) #\:))
      (setf raw (string-left-trim ":" raw))
      (dotnet:invoke tb "set_Text" raw)
      (ignore-errors (dotnet:invoke tb "set_CaretIndex" (length raw))))
    (let ((ms (%palette-matches raw)))
      (setf (viewer-cmd-matches vw) ms)
      (let ((items (dotnet:invoke lb "get_Items")))
        (dotnet:invoke items "Clear")
        (dolist (c ms) (dotnet:invoke items "Add" (%palette-row c))))
      (dotnet:invoke lb "set_IsVisible" (and ms t))
      (when ms (dotnet:invoke lb "set_SelectedIndex" 0)))))

(defun %cmd-strip-leading (vw ch)
  "Strip the prompt character CH (: or /) that slipped into the front of the input field to tidy display."
  (let* ((tb (viewer-cmd-input vw))
         (raw (or (dotnet:invoke tb "get_Text") "")))
    (when (and (plusp (length raw)) (char= (char raw 0) ch))
      (let ((s (string-left-trim (string ch) raw)))
        (dotnet:invoke tb "set_Text" s)
        (ignore-errors (dotnet:invoke tb "set_CaretIndex" (length s)))))))

(defun %cmd-move (vw d)
  ;; Cycle by the actual item count of the list (mode-independent: works correctly for both command/toc lists).
  (let* ((lb (viewer-cmd-list vw))
         (n  (dotnet:invoke (dotnet:invoke lb "get_Items") "get_Count")))
    (when (plusp n)
      (dotnet:invoke lb "set_SelectedIndex"
                     (mod (+ (max 0 (dotnet:invoke lb "get_SelectedIndex")) d) n)))))

(defun %cmd-hide (vw)
  "Close the command bar and return focus to the viewport."
  (dotnet:invoke (viewer-cmd-bar vw) "set_IsVisible" nil)
  (%focus-viewport vw))

(defun %cmd-run (vw)
  (let* ((lb (viewer-cmd-list vw))
         (i  (dotnet:invoke lb "get_SelectedIndex"))
         (c  (and (>= i 0) (nth i (viewer-cmd-matches vw)))))
    (%cmd-hide vw)
    (when c (%run-command vw (command-id c)))))

(defun %command-palette (vw)
  "Open the vi-style : command bar (#23). Filter by input, select with ↑↓/click, Enter to run, Esc to close.
An overlay at the bottom of the main body, not a separate window. While the bar is shown, %on-key is disabled so input does not misfire commands."
  (setf (viewer-cmd-mode vw) :command)
  (let ((tb (viewer-cmd-input vw)))
    (when (viewer-cmd-prompt vw) (dotnet:invoke (viewer-cmd-prompt vw) "set_Text" ":"))
    (dotnet:invoke (viewer-cmd-list vw) "set_IsVisible" t)
    (dotnet:invoke (viewer-cmd-bar vw) "set_IsVisible" t)
    (dotnet:invoke tb "set_Text" "")
    (%cmd-populate vw)
    ;; Focus is delayed by one cycle (prevents the opening key : itself from entering the input field).
    (%ui-post (lambda () (dotnet:invoke tb "Focus")))))

(defun %command-search (vw)
  "Open the vi-style / search bar (#23). Enter searches body/page name → first hit. Then cycle with n/N."
  (setf (viewer-cmd-mode vw) :search)
  (let ((tb (viewer-cmd-input vw)))
    (when (viewer-cmd-prompt vw) (dotnet:invoke (viewer-cmd-prompt vw) "set_Text" "/"))
    (dotnet:invoke (viewer-cmd-list vw) "set_IsVisible" nil)  ; search needs no list (cycle with n/N)
    (dotnet:invoke (viewer-cmd-bar vw) "set_IsVisible" t)
    (dotnet:invoke tb "set_Text" "")
    ;; Focus is delayed by one cycle (prevents the opening key / itself from entering the input field).
    (%ui-post (lambda () (dotnet:invoke tb "Focus")))))

(defun %toc-row (e)
  "One TOC row: depth indent + title + page. e = (depth page title)."
  (destructuring-bind (depth page title) e
    (format nil "~a~a   p~d" (make-string (* 2 (max 0 depth)) :initial-element #\Space)
            title (1+ page))))

(defun %toc-populate (vw)
  "Filter the TOC by title substring match (case-insensitive) from input and update the list."
  (let* ((tb (viewer-cmd-input vw))
         (lb (viewer-cmd-list vw))
         (q  (string-downcase (string-trim " " (or (dotnet:invoke tb "get_Text") ""))))
         (es (if (string= q "")
                 (viewer-toc-entries vw)
                 (remove-if-not (lambda (e) (search q (string-downcase (third e))))
                                (viewer-toc-entries vw)))))
    (setf (viewer-toc-filtered vw) es)
    (let ((items (dotnet:invoke lb "get_Items")))
      (dotnet:invoke items "Clear")
      (dolist (e es) (dotnet:invoke items "Add" (%toc-row e))))
    (dotnet:invoke lb "set_IsVisible" (and es t))
    (when es (dotnet:invoke lb "set_SelectedIndex" 0))))

(defun %toc-run (vw)
  "Jump to the page of the selected TOC entry."
  (let* ((lb (viewer-cmd-list vw))
         (i  (dotnet:invoke lb "get_SelectedIndex"))
         (e  (and (>= i 0) (nth i (viewer-toc-filtered vw)))))
    (%cmd-hide vw)
    (when e
      (let ((page (second e)))
        (when (and page (>= page 0)) (%jump-to-page vw page))))))

(defun %command-toc (vw)
  "Open the TOC bar (#23). Filter titles by input, Enter/click to go to that page. Notify if no TOC."
  (let ((es (ignore-errors (book-outline (viewer-book vw)))))
    (if (null es)
        (%status vw "no TOC (this book has no bookmarks)")
        (progn
          (setf (viewer-cmd-mode vw) :toc (viewer-toc-entries vw) es)
          (let ((tb (viewer-cmd-input vw)))
            (when (viewer-cmd-prompt vw) (dotnet:invoke (viewer-cmd-prompt vw) "set_Text" "≡"))
            (dotnet:invoke (viewer-cmd-list vw) "set_IsVisible" t)
            (dotnet:invoke (viewer-cmd-bar vw) "set_IsVisible" t)
            (dotnet:invoke tb "set_Text" "")
            (%toc-populate vw)
            ;; Focus is delayed by one cycle (prevents the opening key O itself from entering the input field).
            (%ui-post (lambda () (dotnet:invoke tb "Focus"))))))))

(defun %cmd-accept (vw)
  "Confirm the command bar (Enter / double-click). Branch by mode."
  (ecase (viewer-cmd-mode vw)
    (:command (%cmd-run vw))
    (:search  (let ((q (or (dotnet:invoke (viewer-cmd-input vw) "get_Text") "")))
                (%cmd-hide vw) (%run-search vw q)))
    (:toc     (%toc-run vw))))

(defun %make-cmdbar (vw)
  "A vi-style command bar attached to the bottom of the main body (hidden by default). Store controls in viewer and return."
  (let* ((bar (dotnet:new "Avalonia.Controls.Border"))
         (col (dotnet:new "Avalonia.Controls.StackPanel"))
         (lb  (dotnet:new "Avalonia.Controls.ListBox"))
         (row (dotnet:new "Avalonia.Controls.StackPanel"))
         (lbl (dotnet:new "Avalonia.Controls.TextBlock"))
         (tb  (dotnet:new "Avalonia.Controls.TextBox")))
    (dotnet:invoke bar "set_IsVisible" nil)
    ;; A smaller box aligned to the bottom center rather than full width (stretches to fit content).
    (dotnet:invoke bar "set_VerticalAlignment"
                   (dotnet:static "Avalonia.Layout.VerticalAlignment" "Bottom"))
    (dotnet:invoke bar "set_HorizontalAlignment"
                   (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Center"))
    (dotnet:invoke bar "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 0d0 24d0))
    (dotnet:invoke bar "set_Padding" (dotnet:new "Avalonia.Thickness" 8d0 6d0 8d0 6d0))
    (dotnet:invoke bar "set_CornerRadius" (dotnet:new "Avalonia.CornerRadius" 6d0))
    (dotnet:invoke bar "set_Background" (dotnet:static "Avalonia.Media.Brushes" "Black"))
    (dotnet:invoke bar "set_Opacity" 0.92d0)
    (dotnet:invoke lb "set_MaxHeight" 220d0)
    (dotnet:invoke lb "set_Background" (dotnet:static "Avalonia.Media.Brushes" "Transparent"))
    ;; Margin between candidates and input field (prevents the bottom candidate from appearing to overlap the input).
    (dotnet:invoke lb "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 0d0 8d0))
    (dotnet:invoke row "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
    (dotnet:invoke lbl "set_Text" ":")
    (dotnet:invoke lbl "set_Foreground" (dotnet:static "Avalonia.Media.Brushes" "White"))
    (dotnet:invoke lbl "set_VerticalAlignment"
                   (dotnet:static "Avalonia.Layout.VerticalAlignment" "Center"))
    (dotnet:invoke lbl "set_Margin" (dotnet:new "Avalonia.Thickness" 8d0 0d0 4d0 0d0))
    (dotnet:invoke tb "set_BorderThickness" (dotnet:new "Avalonia.Thickness" 0d0))
    (dotnet:invoke tb "set_Background" (dotnet:static "Avalonia.Media.Brushes" "Transparent"))
    (dotnet:invoke tb "set_Foreground" (dotnet:static "Avalonia.Media.Brushes" "White"))
    (dotnet:invoke tb "set_MinWidth" 320d0)
    (dotnet:invoke tb "set_Watermark" "command name (Enter to run / Esc to close)")
    ;; Live filtering: command=command / toc=TOC. search does not live-search, only strips leading /.
    (dotnet:add-event tb "TextChanged"
      (lambda (s e) (declare (ignore s e))
        (ecase (viewer-cmd-mode vw)
          (:command (%cmd-populate vw))      ; filter candidates (also strip leading :)
          (:toc     (%toc-populate vw))      ; filter TOC
          (:search  (%cmd-strip-leading vw #\/)))))  ; search: strip leading / (tidy display)
    ;; Handle keys on the TextBox itself (attaching to Window means Enter does not bubble).
    (dotnet:add-event tb "KeyDown"
      (lambda (s e) (declare (ignore s))
        (let ((kn (dotnet:invoke (dotnet:invoke e "get_Key") "ToString")))
          ;; In Avalonia Enter/Return are equivalent and ToString is "Return". Accept both.
          (cond ((or (string= kn "Return") (string= kn "Enter"))
                 (dotnet:invoke e "set_Handled" t) (%cmd-accept vw))  ; branch by mode
                ((string= kn "Escape") (dotnet:invoke e "set_Handled" t) (%cmd-hide vw))
                ;; Ctrl-n/Ctrl-p to move items (vi-ish; also avoids the single-line TextBox eating ↑↓). Keep ↑↓ too just in case.
                ((and (string= kn "N") (%ctrl? e)) (dotnet:invoke e "set_Handled" t) (%cmd-move vw 1))
                ((and (string= kn "P") (%ctrl? e)) (dotnet:invoke e "set_Handled" t) (%cmd-move vw -1))
                ((string= kn "Down")   (dotnet:invoke e "set_Handled" t) (%cmd-move vw 1))
                ((string= kn "Up")     (dotnet:invoke e "set_Handled" t) (%cmd-move vw -1))))))
    (dotnet:add-event lb "DoubleTapped" (lambda (s e) (declare (ignore s e)) (%cmd-accept vw)))
    (let ((rc (dotnet:invoke row "get_Children")))
      (dotnet:invoke rc "Add" lbl)
      (dotnet:invoke rc "Add" tb))
    (let ((cc (dotnet:invoke col "get_Children")))
      (dotnet:invoke cc "Add" lb)
      (dotnet:invoke cc "Add" row))
    (dotnet:invoke bar "set_Child" col)
    (setf (viewer-cmd-bar vw) bar
          (viewer-cmd-input vw) tb
          (viewer-cmd-list vw) lb
          (viewer-cmd-prompt vw) lbl)
    bar))

(defun %make-status (vw)
  "A small status display shown at the top center of the main body (search count/message; visible even in fullscreen). Hidden by default."
  (let ((box (dotnet:new "Avalonia.Controls.Border"))
        (txt (dotnet:new "Avalonia.Controls.TextBlock")))
    (dotnet:invoke box "set_IsVisible" nil)
    (dotnet:invoke box "set_VerticalAlignment" (dotnet:static "Avalonia.Layout.VerticalAlignment" "Top"))
    (dotnet:invoke box "set_HorizontalAlignment" (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Center"))
    (dotnet:invoke box "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 12d0 0d0 0d0))
    (dotnet:invoke box "set_Padding" (dotnet:new "Avalonia.Thickness" 12d0 6d0 12d0 6d0))
    (dotnet:invoke box "set_CornerRadius" (dotnet:new "Avalonia.CornerRadius" 6d0))
    (dotnet:invoke box "set_Background" (dotnet:static "Avalonia.Media.Brushes" "Black"))
    (dotnet:invoke box "set_Opacity" 0.8d0)
    (dotnet:invoke box "set_IsHitTestVisible" nil)
    (dotnet:invoke txt "set_Foreground" (dotnet:static "Avalonia.Media.Brushes" "White"))
    (dotnet:invoke box "set_Child" txt)
    (setf (viewer-status-box vw) box (viewer-status-text vw) txt)
    box))

(defun %status (vw text)
  "Show TEXT in the status display (visible even in fullscreen with no title bar). Cleared on the next page render."
  (let ((box (viewer-status-box vw)) (txt (viewer-status-text vw)))
    (when (and box txt)
      (dotnet:invoke txt "set_Text" text)
      (dotnet:invoke box "set_IsVisible" t))))

(defun %status-clear (vw)
  (let ((box (viewer-status-box vw)))
    (when box (dotnet:invoke box "set_IsVisible" nil))))

