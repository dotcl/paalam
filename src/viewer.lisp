;;;; viewer.lisp — paalam (#39 main.lisp split): viewer struct, spread display, render
(in-package :paalam)

(defstruct viewer book spreads (sindex 0) window left-img right-img source loader
           viewport scale-xf trans-xf (zoom 1d0) (tx 0d0) (ty 0d0)
           (fit-base 1d0)               ; fit base scale (effective = fit-base*zoom; #15)
           (content-w 0d0) (content-h 0d0)  ; content dimensions of current spread (DIP; #15)
           (rotation 0) rot-xf          ; rotation angle (degrees) and RotateTransform (#28)
           (slide-timer nil)            ; slideshow DispatcherTimer (nil=stopped; #28)
           (loupe nil) (loupe-tx0 0d0) (loupe-ty0 0d0)  ; loupe state and pan base at fit (#28)
           (loupe-cx 0d0) (loupe-cy 0d0)                ; loupe current center (for arrow-key move; #28)
           (dragging nil) (lx 0d0) (ly 0d0)
           clip-pane clip-stack         ; clip pane (#18 Phase 2)
           clip-combo                   ; clip selection combo (#35 ③)
           clip-scroll                  ; clip pane ScrollViewer (for cursor follow; #35 ④)
           (clip-cursor 0)              ; vi cursor position for clip editing (#35 ④)
           (clip-selected nil)          ; multiple selection (list of indexes; #35 ④ Phase 2)
           (clip-pending nil)           ; vi multi-key wait (:d=dd / :y=yy; #35 ④ Phase 2)
           (clip-count nil)             ; vi count prefix (#35 ④ Phase 2)
           thumb-pane thumb-stack       ; page thumbnail bookshelf (#6 A)
           browse-pane browse-tree      ; folder tree (#6 B)
           info-pane info-stack         ; file info / EXIF panel (#36)
           (info-page nil)              ; page selected for the info pane (left/right pick in spread; #36)
           (reorder-from nil)           ; source index during drag reorder (Phase 3)
           (reorder-img nil)            ; thumbnail being dragged (to restore opacity; #20)
           (reorder-line nil)           ; insertion position indicator Border (#20)
           (dnd-overlay nil)            ; DnD drop-allowed highlight frame (#20)
           (pending-count nil)          ; vi-style count prefix (digit-key accumulation; #23)
           (pending-g nil)              ; vi-style multi-key g wait (gg=first; #23)
           cmd-bar cmd-input cmd-list cmd-prompt         ; : command bar / search bar (#23)
           (cmd-matches nil) (cmd-mode :command)         ; cmd-mode = :command / :search
           (search-matches nil) (search-index 0)         ; / full-text search: list of hit page indexes (#23)
           (search-query nil) (search-hl nil)            ; search term / highlight rectangle (overlay)
           (page-offsets nil)                            ; current spread page→(xoff yoff) (for highlight placement)
           (toc-entries nil) (toc-filtered nil)          ; table of contents (depth page title) all / filtered (#23)
           status-box status-text                        ; in-window status display (visible even in fullscreen; #23)
           menu                         ; menu bar (#21; hidden in fullscreen)
           (gesturing nil) (gseq "") (glx 0d0) (gly 0d0)  ; mouse gesture: right-dragging/direction sequence/last point (#29)
           (gesture-fired nil))         ; gesture fired on last right release → suppress ContextMenu (#29)

(defun %reopen (vw)
  "Re-open source and swap book/loader/spreads (recursion toggle, etc.). Current page is kept.
When source is nil (empty window / during clip preview), do nothing (#35 ⑤)."
  (unless (viewer-source vw) (return-from %reopen))
  (let ((keep (%cur-page vw))
        (old-loader (viewer-loader vw)) (old-book (viewer-book vw)))
    (multiple-value-bind (book start) (open-book (viewer-source vw))
      (declare (ignore start))
      (setf *page-direction* (%book-direction book))   ; binding direction by type (#33)
      (ignore-errors (loader-quiesce old-loader))   ; stop only the thread (Bitmap is freed after render)
      (setf (viewer-search-matches vw) nil (viewer-search-index vw) 0)  ; search results invalid when book changes
      (%slideshow-stop vw)                       ; stop slideshow on book switch (#28)
      (setf (viewer-rotation vw) 0)              ; reset rotation too
      (setf (viewer-book vw) book
            (viewer-loader vw) (open-loader
                                (let ((p (viewer-source vw))) (lambda () (open-book p)))
                                (lambda (index)
                                  (%ui-post (lambda () (%on-page-loaded vw index)))))
            (viewer-spreads vw) (compute-spreads (book-page-count book))
            (viewer-sindex vw) (spread-index-of (viewer-spreads vw)
                                                (min keep (max 0 (1- (book-page-count book))))))
      (show-spread vw)
      (%thumb-pane-refresh vw)
      (%defer-free old-loader old-book))))

(defun open-path (vw path)
  "Open another book (DnD, etc.) and swap. Resume at history page if any, else from start.
Call this on the UI thread."
  (handler-case
      (multiple-value-bind (book start) (open-book path)
        (setf *page-direction* (%book-direction book))   ; binding direction by type (#33)
        (let ((old-loader (viewer-loader vw)) (old-book (viewer-book vw)))
          (ignore-errors (loader-quiesce old-loader))   ; stop only the thread (Bitmap is freed after render)
          (let* ((spreads (compute-spreads (book-page-count book)))
                 (resume (history-page path))
                 (page (if resume (min resume (max 0 (1- (book-page-count book)))) start)))
            (setf (viewer-source vw) path
                  (viewer-book vw) book
                  (viewer-loader vw) (open-loader
                                      (let ((p path)) (lambda () (open-book p)))
                                      (lambda (i) (%ui-post (lambda () (%on-page-loaded vw i)))))
                  (viewer-spreads vw) spreads
                  (viewer-sindex vw) (spread-index-of spreads page))
            (show-spread vw)
            (%thumb-pane-refresh vw)
            (%defer-free old-loader old-book))))
    (error (e) (log-err "open-path" e))))

(defun apply-transform (vw)
  ;; Transform order: rotate (around content center) → scale (origin) → translate. Effective scale = fit base × zoom.
  (when (viewer-rot-xf vw)
    (dotnet:invoke (viewer-rot-xf vw) "set_Angle" (coerce (viewer-rotation vw) 'double-float))
    (dotnet:invoke (viewer-rot-xf vw) "set_CenterX" (/ (viewer-content-w vw) 2d0))
    (dotnet:invoke (viewer-rot-xf vw) "set_CenterY" (/ (viewer-content-h vw) 2d0)))
  (let ((s (* (viewer-fit-base vw) (viewer-zoom vw))))
    (dotnet:invoke (viewer-scale-xf vw) "set_ScaleX" s)
    (dotnet:invoke (viewer-scale-xf vw) "set_ScaleY" s))
  (dotnet:invoke (viewer-trans-xf vw) "set_X" (viewer-tx vw))
  (dotnet:invoke (viewer-trans-xf vw) "set_Y" (viewer-ty vw)))

(defun %apply-background (vw)
  "Set the image display area (= margins/letterbox at fit) background to *background-color* (#2).
Paint the viewport, not the whole window. This way chrome like menus stays the theme default,
so menu text stays readable even with a white background."
  (ignore-errors
    (dotnet:invoke (viewer-viewport vw) "set_Background"
                   (dotnet:static "Avalonia.Media.Brushes" *background-color*))))

(defun %theme-variant (kw)
  "Theme keyword → Avalonia.Styling.ThemeVariant static value. :system=Default (follows OS)."
  (dotnet:static "Avalonia.Styling.ThemeVariant"
                 (case kw (:light "Light") (:dark "Dark") (t "Default"))))

(defun %apply-theme ()
  "Apply *theme* to Application.RequestedThemeVariant (affects all window chrome; #27).
:system is Default = follows OS light/dark."
  (ignore-errors
    (let ((app (dotnet:static "Avalonia.Application" "Current")))
      (when app
        (dotnet:invoke app "set_RequestedThemeVariant" (%theme-variant *theme*))))))

(defun %apply-interpolation (vw)
  "Set zoom interpolation quality on left/right images (#2). :none=nearest (for pixel art)/:low/:high."
  (let ((mode (dotnet:static "Avalonia.Media.Imaging.BitmapInterpolationMode"
                             (case *interpolation*
                               (:none "None") (:low "LowQuality") (t "HighQuality")))))
    (dolist (img (list (viewer-left-img vw) (viewer-right-img vw)))
      (ignore-errors
        (dotnet:static "Avalonia.Media.RenderOptions" "SetBitmapInterpolationMode" img mode)))))

(defun %viewport-size (vw)
  "Current viewport size (DIP). 0 before layout."
  (let ((b (dotnet:invoke (viewer-viewport vw) "get_Bounds")))
    (values (dotnet:invoke b "get_Width") (dotnet:invoke b "get_Height"))))

(defun %fit-base (cw ch vpw vph)
  "Base scale to fit content (cw×ch) into viewport (vpw×vph) (per mode)."
  (if (or (<= cw 0) (<= ch 0) (<= vpw 0) (<= vph 0))
      1d0
      (ecase *fit-mode*
        (:whole  (min (/ vpw cw) (/ vph ch)))
        (:width  (/ vpw cw))
        (:height (/ vph ch)))))

(defun %apply-fit (vw)
  "Determine base scale and anchor (tx/ty) from current content dimensions and viewport, then apply.
whole=centered, width/height=top-left anchor (overflow scrolls via pan). User zoom is preserved.
At rotation 90/270, swap effective dimensions and place to fit the rotated bbox (around content center) (#28)."
  (multiple-value-bind (vpw vph) (%viewport-size vw)
    (let* ((cw (viewer-content-w vw)) (ch (viewer-content-h vw))
           (rot (mod (viewer-rotation vw) 360))
           (swap (or (= rot 90) (= rot 270)))
           (ew (if swap ch cw)) (eh (if swap cw ch))   ; effective dimensions after rotation
           (base (%fit-base ew eh vpw vph))
           (s (* base (viewer-zoom vw))))
      (setf (viewer-fit-base vw) base)
      ;; The rotated bbox is ew×eh centered on the content center (cw/2,ch/2). Its top-left in content
      ;; coords = ((cw-ew)/2,(ch-eh)/2). Translate to the placement target in the viewport (center if room / start if overflow).
      (let ((target-x (max 0d0 (/ (- vpw (* ew s)) 2d0)))
            (target-y (max 0d0 (/ (- vph (* eh s)) 2d0))))
        (setf (viewer-tx vw) (- target-x (* (/ (- cw ew) 2d0) s))
              (viewer-ty vw) (- target-y (* (/ (- ch eh) 2d0) s))))
      (apply-transform vw))))

(defun reset-view (vw)
  "Reset zoom to 1x and re-fit in the current mode."
  (setf (viewer-zoom vw) 1d0)
  (%apply-fit vw))

(defun compute-spreads (n)
  "Split n pages into spread units per *spread* / *first-page-single*."
  (let ((spreads '()) (i 0))
    (cond
      ((<= n 0))
      ((not *spread*)
       (loop for k from 0 below n do (push (list k) spreads)))
      (t
       (when *first-page-single*
         (push (list 0) spreads) (setf i 1))
       (loop while (< i n) do
         (if (< (1+ i) n)
             (progn (push (list i (1+ i)) spreads) (incf i 2))
             (progn (push (list i) spreads) (incf i))))))
    (coerce (nreverse spreads) 'vector)))

(defun spread-index-of (spreads page)
  "Index of the spread containing page (0 if not found)."
  (or (position-if (lambda (pages) (member page pages)) spreads) 0))

(defun %bmp-size (bmp)
  "Bitmap DIP size (values w h)."
  (let ((s (dotnet:invoke bmp "get_Size")))
    (values (dotnet:invoke s "get_Width") (dotnet:invoke s "get_Height"))))

(defun %canvas-place (img bmp x y)
  "Set bmp on img and place at (x,y) on the Canvas at native dimensions (bmp=nil hides it)."
  (dotnet:invoke img "set_Source" bmp)
  (dotnet:invoke img "set_IsVisible" (and bmp t))
  (when bmp
    (dotnet:static "Avalonia.Controls.Canvas" "SetLeft" img (coerce x 'double-float))
    (dotnet:static "Avalonia.Controls.Canvas" "SetTop"  img (coerce y 'double-float))))

(defun %fit-label ()
  (case *fit-mode* (:width " [W]") (:height " [H]") (t "")))

(defun %spread-title (vw pages)
  (let* ((book (viewer-book vw)) (n (book-page-count book))
         (path (viewer-source vw))
         (star (if (some (lambda (p) (bookmarked-p path p)) pages) "★ " ""))
         ;; #35 ③: current clip name and count (when multiple clips or non-empty).
         (clip (if (or *clip* (> (length *clips*) 1))
                   (format nil " [clip ~a:~d]" (clip-current-name) (length *clip*)) ""))
         (fit  (%fit-label)))
    (if (= (length pages) 1)
        (format nil "~apaalam — ~a (~d/~d)~a~a"
                star (book-page-name book (first pages)) (1+ (first pages)) n clip fit)
        (destructuring-bind (lo hi) pages
          (format nil "~apaalam — ~a | ~a (~d-~d/~d)~a~a"
                  star (book-page-name book lo) (book-page-name book hi)
                  (1+ lo) (1+ hi) n clip fit)))))

(defun %page-bitmap (vw index) (and index (loader-get (viewer-loader vw) index)))

(defun %render-spread (vw)
  "Place the current spread from cache onto the Canvas, update content dimensions → apply fit.
Unloaded pages are blank (filled later by %on-page-loaded). A spread matches left/right by binding
direction; vertically centered to the taller one. content width = sum of left/right widths, height = taller one (#15)."
  (let* ((spreads (viewer-spreads vw)) (m (length spreads)))
    (when (plusp m)
      (let* ((si (viewer-sindex vw))
             (pages (aref spreads si))
             (li (viewer-left-img vw))
             (ri (viewer-right-img vw)))
        (%clear-highlights vw)                ; page changes, so clear the previous search highlight
        (%status-clear vw)                    ; clear status message too on page move
        (setf (viewer-page-offsets vw) nil)
        (if (= (length pages) 1)
            (let ((bmp (%page-bitmap vw (first pages))))
              (multiple-value-bind (w h) (if bmp (%bmp-size bmp) (values 0d0 0d0))
                (%canvas-place li bmp 0d0 0d0)
                (dotnet:invoke ri "set_IsVisible" nil)
                (setf (viewer-content-w vw) w (viewer-content-h vw) h)
                (setf (viewer-page-offsets vw) (list (list (first pages) 0d0 0d0 w h)))))
            (destructuring-bind (lo hi) pages
              ;; rtl (RTL): put lower index=lo on the right, hi on the left. ltr is reversed.
              (let* ((left-page  (if (eq *page-direction* :rtl) hi lo))
                     (right-page (if (eq *page-direction* :rtl) lo hi))
                     (lb (%page-bitmap vw left-page))
                     (rb (%page-bitmap vw right-page)))
                (multiple-value-bind (lw lh) (if lb (%bmp-size lb) (values 0d0 0d0))
                  (multiple-value-bind (rw rh) (if rb (%bmp-size rb) (values 0d0 0d0))
                    (let ((ch (max lh rh)))
                      (%canvas-place li lb 0d0 (/ (- ch lh) 2d0))
                      (%canvas-place ri rb lw  (/ (- ch rh) 2d0))
                      (setf (viewer-content-w vw) (+ lw rw)
                            (viewer-content-h vw) ch)
                      (setf (viewer-page-offsets vw)
                            (list (list left-page  0d0 (/ (- ch lh) 2d0) lw lh)
                                  (list right-page lw  (/ (- ch rh) 2d0) rw rh)))))))))
        (%apply-fit vw)))))

(defun %update-title (vw)
  ;; The title shows content info only. Messages like search hit counts go to %status (in-window)
  ;; (because the title bar is not visible in fullscreen).
  (let ((spreads (viewer-spreads vw)))
    (when (plusp (length spreads))
      (dotnet:invoke (viewer-window vw) "set_Title"
                     (%spread-title vw (aref spreads (viewer-sindex vw)))))))

(defun %goto-bookmark (vw dir)
  "Go to the nearest bookmark page of the current book in the dir direction from the current page."
  (let* ((cur (%cur-page vw))
         (pages (book-bookmark-pages (viewer-source vw)))
         (target (if (> dir 0)
                     (find-if (lambda (p) (> p cur)) pages)
                     (find-if (lambda (p) (< p cur)) (reverse pages)))))
    (when target
      (setf (viewer-sindex vw) (spread-index-of (viewer-spreads vw) target))
      (show-spread vw))))

(defun %load-order (vw)
  "Page indexes of the current spread + bidirectional prefetch, in priority order (#25).
Order is current → +1 → -1 → +2..+ahead → -2..-behind, forward-first. behind=min(2,ahead)."
  (let* ((spreads (viewer-spreads vw)) (m (length spreads)) (si (viewer-sindex vw))
         (ahead (max 0 *prefetch-ahead*)) (behind (min 2 ahead))
         (ks '()))
    (when (plusp m)
      (push 0 ks)
      (when (>= ahead 1) (push 1 ks))
      (when (>= behind 1) (push -1 ks))
      (loop for k from 2 to ahead do (push k ks))
      (loop for k from 2 to behind do (push (- k) ks))
      (let ((acc '()))
        (dolist (k (nreverse ks))                       ; [0 1 -1 2..ahead -2..-behind]
          (let ((s (+ si k)))
            (when (and (>= s 0) (< s m))
              (dolist (p (aref spreads s)) (push p acc)))))
        (nreverse acc)))))

(defun show-spread (vw)
  "Display the spread (sindex clamped). Request the needed pages from the loader and render from cache,
and prefetch-request the next spread in reading direction too."
  (setf (viewer-loupe vw) nil)          ; loupe is released on page move (#28)
  (let* ((spreads (viewer-spreads vw)) (m (length spreads)))
    (when (plusp m)
      (let ((si (max 0 (min (viewer-sindex vw) (1- m)))))
        (setf (viewer-sindex vw) si)
        ;; The LRU center is the *page* index (not the spread index — otherwise eviction distance is off).
        (loader-request (viewer-loader vw) (%load-order vw) (%cur-page vw))
        (%dbg "show si=~a pages=~a hit=~a" si (aref spreads si)
              (mapcar (lambda (p) (if (%page-bitmap vw p) 1 0)) (aref spreads si)))
        (%render-spread vw)
        (%update-title vw)
        (%info-pane-refresh-if-visible vw)   ; refresh info panel on page move (#36)
        (history-update (viewer-source vw) (%cur-page vw))   ; in-memory; saved on exit.
        (reset-view vw)))))

(defun %on-page-loaded (vw index)
  "Worker finished loading index → if it belongs to the current spread, re-render from cache
(zoom/pan kept; no reset-view)."
  (let* ((spreads (viewer-spreads vw)) (m (length spreads)))
    (when (plusp m)
      (let ((pages (aref spreads (viewer-sindex vw))))
        (when (member index pages)
          (%render-spread vw)
          (%info-pane-refresh-if-visible vw))))))   ; dims become available once decoded (#36)

(defun %recompute (vw keep-page)
  "Recompute spreads, position to the spread containing keep-page, and re-render."
  (setf (viewer-spreads vw) (compute-spreads (book-page-count (viewer-book vw))))
  (setf (viewer-sindex vw) (spread-index-of (viewer-spreads vw) keep-page))
  (show-spread vw))

(defun %cur-page (vw)
  (let ((sp (viewer-spreads vw)))
    (if (plusp (length sp)) (first (aref sp (viewer-sindex vw))) 0)))

(defparameter *zoom-step* 1.2d0 "Zoom factor per wheel notch.")
(defparameter *zoom-min* 0.1d0)
(defparameter *zoom-max* 10d0)

(defun %pos-xy (e elem)
  "PointerEventArgs coordinates relative to elem as (values x y)."
  (let ((p (dotnet:invoke e "GetPosition" elem)))
    (values (dotnet:invoke p "get_X") (dotnet:invoke p "get_Y"))))

(defun %on-wheel (vw e)
  "Wheel zoom. Zoom in/out with the cursor position as the fixed point."
  (let* ((dy (dotnet:invoke (dotnet:invoke e "get_Delta") "get_Y"))
         (s  (viewer-zoom vw))
         (s2 (max *zoom-min* (min *zoom-max* (* s (if (> dy 0) *zoom-step* (/ 1d0 *zoom-step*)))))))
    (multiple-value-bind (cx cy) (%pos-xy e (viewer-viewport vw))
      ;; t' = c - (s'/s)(c - t)  (origin top-left, screen = s*p + t)
      (let ((r (/ s2 s)))
        (setf (viewer-tx vw) (- cx (* r (- cx (viewer-tx vw))))
              (viewer-ty vw) (- cy (* r (- cy (viewer-ty vw))))
              (viewer-zoom vw) s2)))
    (apply-transform vw)))

(defun %left-pressed-p (vw e)
  "Whether the left button is pressed right now (PointerPoint.Properties)."
  (dotnet:invoke (dotnet:invoke (dotnet:invoke e "GetCurrentPoint" (viewer-viewport vw))
                                "get_Properties")
                 "get_IsLeftButtonPressed"))

(defun %right-pressed-p (vw e)
  "Whether the right button is pressed right now (PointerPoint.Properties)."
  (dotnet:invoke (dotnet:invoke (dotnet:invoke e "GetCurrentPoint" (viewer-viewport vw))
                                "get_Properties")
                 "get_IsRightButtonPressed"))

;;; Mouse gesture (#29): right-button drag direction sequence (U/D/L/R, e.g. "DL") → command.
;;; A pure right click (no move) is the usual context menu. Suppress ContextMenu only when fired.
(defparameter *gesture-threshold* 28d0 "Minimum move to commit one direction (px).")
(defparameter *default-gestures*
  '(("L" . :reading-fwd) ("R" . :reading-bwd)
    ("U" . :first-spread) ("D" . :last-spread))
  "Factory-default mouse gestures (right-drag direction sequence → command id). Left/right are read
forward/backward (follows binding direction), up/down are first/last.")
(defparameter *gestures* (copy-tree *default-gestures*)
  "Current mouse gesture assignment ((direction-sequence . command-id) ...). Persisted to gestures.lisp.")
(defun load-gestures ()
  (let ((d (%read-state "gestures.lisp"))) (when (and (listp d) d) (setf *gestures* d))))
(defun save-gestures () (%write-state "gestures.lisp" *gestures*))

(defun %gesture-start (vw e)
  (multiple-value-bind (x y) (%pos-xy e (viewer-viewport vw))
    (setf (viewer-gesturing vw) t (viewer-gseq vw) ""
          (viewer-glx vw) x (viewer-gly vw) y
          (viewer-gesture-fired vw) nil)))

(defun %gesture-step (seq lx ly x y)
  "Incorporate point (X,Y) into direction sequence SEQ and last point (LX,LY). If moved at least the
threshold, determine the direction (U/D/L/R) and append if different from the previous. Returns
(values seq lx ly moved-p) (when moved-p=t, lx,ly are updated to x,y). Shared by the viewer body and the editing UI record box."
  (let ((dx (- x lx)) (dy (- y ly)))
    (if (>= (max (abs dx) (abs dy)) *gesture-threshold*)
        (let* ((dir (if (> (abs dx) (abs dy))
                        (if (> dx 0d0) "R" "L")
                        (if (> dy 0d0) "D" "U")))
               (seq2 (if (or (zerop (length seq))
                             (not (char= (char seq (1- (length seq))) (char dir 0))))
                         (concatenate 'string seq dir) seq)))
          (values seq2 x y t))
        (values seq lx ly nil))))

(defun %gesture-move (vw e)
  (multiple-value-bind (x y) (%pos-xy e (viewer-viewport vw))
    (multiple-value-bind (seq lx ly moved)
        (%gesture-step (viewer-gseq vw) (viewer-glx vw) (viewer-gly vw) x y)
      (when moved
        ;; Set "gesture fired" as soon as it moves (so we can reliably suppress even if ContextMenu's
        ;; Opening runs before the release; #29).
        (setf (viewer-gseq vw) seq (viewer-glx vw) lx (viewer-gly vw) ly
              (viewer-gesture-fired vw) t)))))

(defun %gesture-end (vw)
  "End of right drag. If there is a direction sequence, run the command and suppress the context menu."
  (when (viewer-gesturing vw)
    (setf (viewer-gesturing vw) nil)
    (let ((seq (viewer-gseq vw)))
      (when (plusp (length seq))
        (setf (viewer-gesture-fired vw) t)   ; ContextRequested suppression flag
        (%status vw (format nil "Gesture: ~a" seq))
        (let ((id (cdr (assoc seq *gestures* :test #'string-equal))))
          (when id (%run-command vw id)))))))

(defun %on-press (vw e)
  ;; Pan starts only with the left button. Don't accidentally enter drag state on menu, etc. presses.
  ;; Don't pan while the loupe is active (movement is handled by cursor follow).
  (cond
    ((and (not (viewer-loupe vw)) (%left-pressed-p vw e))
     (multiple-value-bind (x y) (%pos-xy e (viewer-viewport vw))
       (setf (viewer-dragging vw) t (viewer-lx vw) x (viewer-ly vw) y)))
    ((%right-pressed-p vw e)            ; right drag = gesture start (#29)
     (%gesture-start vw e))))

(defun %on-move (vw e)
  (when (viewer-gesturing vw)           ; accumulate direction while gesturing (#29)
    (if (%right-pressed-p vw e)
        (progn (%gesture-move vw e) (return-from %on-move))
        (%gesture-end vw)))             ; self-heal for a missed release
  (when (viewer-loupe vw) (%loupe-follow vw e) (return-from %on-move))
  (when (viewer-dragging vw)
    ;; Even if a release is missed and dragging lingers, self-heal stops it once the button is up
    ;; (covers the case where PointerReleased doesn't return to main from a menu/another window dialog).
    (if (%left-pressed-p vw e)
        (multiple-value-bind (x y) (%pos-xy e (viewer-viewport vw))
          (incf (viewer-tx vw) (- x (viewer-lx vw)))
          (incf (viewer-ty vw) (- y (viewer-ly vw)))
          (setf (viewer-lx vw) x (viewer-ly vw) y)
          (apply-transform vw))
        (setf (viewer-dragging vw) nil))))

(defun %on-release (vw e) (declare (ignore e))
  (%gesture-end vw)                     ; commit gesture on right drag end (#29)
  (setf (viewer-dragging vw) nil))

(defun %fullscreen-p (vw)
  (string= (dotnet:invoke (dotnet:invoke (viewer-window vw) "get_WindowState") "ToString")
           "FullScreen"))

(defun %set-fullscreen (vw on)
  (dotnet:invoke (viewer-window vw) "set_WindowState"
                 (dotnet:static "Avalonia.Controls.WindowState" (if on "FullScreen" "Normal")))
  ;; In fullscreen, hide the menu bar for immersion (DockPanel stretches root to full screen).
  (when (viewer-menu vw)
    (dotnet:invoke (viewer-menu vw) "set_IsVisible" (not on))))

(defun %quit (vw) (dotnet:invoke (viewer-window vw) "Close"))

;;; Rotate (#28). Rotate the stage's RotateTransform; %apply-fit swaps dimensions and places at 90/270.
(defun %rotate (vw deg)
  (setf (viewer-rotation vw) (mod (+ (viewer-rotation vw) deg) 360))
  (%apply-fit vw)
  (%status vw (format nil "Rotate ~d°" (viewer-rotation vw))))
(defun %rotate-right (vw) (%rotate vw 90))
(defun %rotate-left  (vw) (%rotate vw -90))

;;; Slideshow (#28). Auto-advance via DispatcherTimer, stop at the end.
(defun %slideshow-stop (vw)
  (when (viewer-slide-timer vw)
    (ignore-errors (dotnet:invoke (viewer-slide-timer vw) "Stop"))
    (setf (viewer-slide-timer vw) nil)))
(defun %slideshow-toggle (vw)
  "Start/stop the slideshow. Advance to the next spread every *slideshow-interval* seconds, stop at the end."
  (if (viewer-slide-timer vw)
      (progn (%slideshow-stop vw) (%status vw "Slideshow stopped"))
      (let ((timer (dotnet:new "Avalonia.Threading.DispatcherTimer")))
        (dotnet:invoke timer "set_Interval"
                       (dotnet:static "System.TimeSpan" "FromSeconds"
                                      (coerce (max 1 *slideshow-interval*) 'double-float)))
        (dotnet:add-event timer "Tick"
          (lambda (s e) (declare (ignore s e))
            (let ((m (length (viewer-spreads vw))) (si (viewer-sindex vw)))
              (if (< si (1- m)) (%next-spread vw) (%slideshow-stop vw)))))
        (dotnet:invoke timer "Start")
        (setf (viewer-slide-timer vw) timer)
        (%status vw (format nil "Slideshow (~ds interval)" *slideshow-interval*)))))

;;; Loupe (magnifier) (#28). Full-view type: magnify the content at the cursor position by *loupe-scale*
;;; and follow cursor movement. Recompute with a cursor-fixed formula from the fit pan base (loupe-tx0/ty0).
(defun %loupe-apply (vw cx cy)
  "Magnify the fit-time content at viewport coords (cx,cy) in place by *loupe-scale* and place it."
  (let ((f *loupe-scale*))
    (setf (viewer-loupe-cx vw) cx (viewer-loupe-cy vw) cy
          (viewer-zoom vw) f
          (viewer-tx vw) (- cx (* (- cx (viewer-loupe-tx0 vw)) f))
          (viewer-ty vw) (- cy (* (- cy (viewer-loupe-ty0 vw)) f)))
    (apply-transform vw)))

(defun %loupe-nudge (vw dx dy)
  "Move the loupe center with arrow keys (instead of page advance; #28)."
  (%loupe-apply vw (+ (viewer-loupe-cx vw) dx) (+ (viewer-loupe-cy vw) dy)))

(defun %loupe-follow (vw e)
  (multiple-value-bind (cx cy) (%pos-xy e (viewer-viewport vw)) (%loupe-apply vw cx cy)))

(defun %loupe-toggle (vw)
  "Loupe ON/OFF. On ON, reset to fit (zoom 1) to fix the pan base → start magnifying at center."
  (if (viewer-loupe vw)
      (progn (setf (viewer-loupe vw) nil) (reset-view vw) (%status vw "Loupe OFF"))
      (progn
        (reset-view vw)                                  ; back to fit to fix tx/ty
        (setf (viewer-loupe-tx0 vw) (viewer-tx vw)
              (viewer-loupe-ty0 vw) (viewer-ty vw)
              (viewer-loupe vw) t)
        (multiple-value-bind (vpw vph) (%viewport-size vw)
          (%loupe-apply vw (/ vpw 2d0) (/ vph 2d0)))     ; initially magnify at center
        (%status vw (format nil "Loupe ON (~,1fx) — move with mouse/arrows / Z to turn OFF" *loupe-scale*)))))

