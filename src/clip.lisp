;;;; clip.lisp — paalam (#39 main.lisp split): clip (#18/#35)
(in-package :paalam)

;;; clip (#18 Phase 1): ((source . page) ...) in add order. For zip export.
;;; #35 ③: named multiple clips. *clips* is the registry, *clip* is the current contents
;;; (existing code keeps using *clip* directly, and save-clip writes back to the current one — minimal-churn design).
(defparameter *clip* nil "Current clip contents ((source . page) ...). Synced with the current element of *clips*.")
(defparameter *clips* (list (cons "Default" nil))
  "Registry of named clips ((name . items) ...). #35 ③")
(defparameter *current-clip* 0 "Current index within *clips*.")
(defun clip-current-cell () (nth (max 0 (min *current-clip* (1- (length *clips*)))) *clips*))
(defun clip-current-name () (car (clip-current-cell)))
(defun %clip-sync () "Write the current *clip* back to the current element of *clips*."
  (setf (cdr (clip-current-cell)) *clip*))
;; Thumbnail memory cache (source.page -> Bitmap). So we don't re-fetch from source
;; each time the pane opens (helps on NAS). Distinct from the loader's cache (the loader
;; Disposes on evict, so it isn't shared).
(defparameter *thumb-cache* (make-hash-table :test 'equal))
(defun %thumb-get (src page) (gethash (cons src page) *thumb-cache*))
(defun %thumb-put (src page bmp) (setf (gethash (cons src page) *thumb-cache*) bmp))
(defun save-clip ()
  "Save the whole registry + current index to clips.lisp (sync the current *clip* first; #35 ③)."
  (%clip-sync)
  (%write-state "clips.lisp" (cons *current-clip* *clips*)))
(defun load-clip ()
  "Prefer clips.lisp. If absent, migrate the old clip.lisp into the default clip (#35 ③)."
  (let ((d (%read-state "clips.lisp")))
    (if (and (consp d) (integerp (car d)) (consp (cdr d)) (every #'consp (cdr d)))
        (setf *current-clip* (car d) *clips* (cdr d))
        (let ((old (%read-state "clip.lisp")))           ; old single clip → default clip
          (setf *clips* (list (cons "Default" (and (listp old) old)))
                *current-clip* 0))))
  (unless (and (consp *clips*) (every #'consp *clips*)) (setf *clips* (list (cons "Default" nil))))
  (setf *current-clip* (max 0 (min *current-clip* (1- (length *clips*))))
        *clip* (cdr (clip-current-cell))))
(defun clip-add (source page)
  "Add (source . page) to the clip (skip duplicates)."
  (when (and source
             (not (member-if (lambda (e) (and (string-equal (car e) source) (eql (cdr e) page)))
                             *clip*)))
    (setf *clip* (append *clip* (list (cons source page))))
    (save-clip)))
(defun clip-clear ()
  ;; Empty the current clip. Only sweep caches that are no longer referenced (keep other clips' entries; #35 ③).
  (setf *clip* nil) (clrhash *thumb-cache*) (save-clip) (%clip-cache-gc))

;;; Local cache for clips (#35 ①). Copy the original bytes of clipped pages once into
;;; clip-cache/ under state, and on export fill from local instead of re-reading the NAS.
;;; Manifest *clip-cache*: (source . page) -> (relative-file-name page-name). Persisted to clip-cache.lisp.
;;; Existing clip.lisp (no cache) falls back to reading from source on export, so it stays compatible.
(defparameter *clip-cache* (make-hash-table :test 'equal))
(defun %clip-cache-dir () (%state-file "clip-cache/"))
(defun %fnv1a-hex (string)
  "FNV-1a 64bit to hex (for cache file names; no .NET crypto dependency. Collisions are negligible in practice)."
  (let ((h 14695981039346656037))
    (loop for ch across string
          do (setf h (logand (* (logxor h (char-code ch)) 1099511628211) #xFFFFFFFFFFFFFFFF)))
    (format nil "~16,'0x" h)))
(defun load-clip-cache ()
  (clrhash *clip-cache*)
  (let ((d (%read-state "clip-cache.lisp")))
    (when (listp d)
      (dolist (e d) (when (and (consp e) (consp (car e)))
                      (setf (gethash (car e) *clip-cache*) (cdr e)))))))
(defun save-clip-cache ()
  (let (al) (maphash (lambda (k v) (push (cons k v) al)) *clip-cache*)
       (%write-state "clip-cache.lisp" al)))
(defun %clip-cached (source page)
  "If cached, (values absolute-path page-name). Otherwise nil."
  (let ((v (gethash (cons source page) *clip-cache*)))
    (when (consp v)
      (let ((p (concatenate 'string (%clip-cache-dir) (first v))))
        (when (%file-exists p)
          (values p (second v)))))))
(defun %clip-cache-drop (item)
  "Delete the cache file and manifest entry for ITEM = (source . page) (when a clip item is deleted)."
  (let ((v (gethash item *clip-cache*)))
    (when (consp v)
      (ignore-errors
        (let ((p (concatenate 'string (%clip-cache-dir) (first v))))
          (when (%file-exists p)
            (dotnet:static "System.IO.File" "Delete" p))))
      (remhash item *clip-cache*)
      (save-clip-cache))))
(defun %clip-cache-clear ()
  "Discard the entire cache (on clip-clear)."
  (ignore-errors
    (when (%dir-exists (%clip-cache-dir))
      (dotnet:static "System.IO.Directory" "Delete" (%clip-cache-dir) t)))
  (clrhash *clip-cache*)
  (save-clip-cache))
(defun %clip-cache-warm ()
  "Copy uncached clip items to local in the background. Open once per source, batched,
to minimize re-opening on the NAS. Idempotent (skip what already exists)."
  (make-thread
   (lambda ()
     (handler-case
         (let ((by-source (make-hash-table :test 'equal)) (changed nil))
           (dolist (item *clip*)
             (unless (gethash item *clip-cache*)
               (push (cdr item) (gethash (car item) by-source))))
           (maphash
            (lambda (source pages)
              (handler-case
                  (let ((book (open-book source)))
                    (unwind-protect
                         (dolist (page pages)
                           (handler-case
                               (multiple-value-bind (bytes ext) (book-page-bytes book page)
                                 (let* ((name (concatenate 'string
                                                           (%fnv1a-hex (format nil "~a#~d" source page)) "." ext))
                                        (path (concatenate 'string (%clip-cache-dir) name)))
                                   (dotnet:static "System.IO.Directory" "CreateDirectory" (%clip-cache-dir))
                                   (dotnet:static "System.IO.File" "WriteAllBytes" path bytes)
                                   (setf (gethash (cons source page) *clip-cache*)
                                         (list name (book-page-name book page))
                                         changed t)))
                             (error (e) (log-err "clip-cache-page" e))))
                      (ignore-errors (book-close book))))
                (error (e) (log-err "clip-cache-source" e))))
            by-source)
           (when changed (save-clip-cache)))
       (error (e) (log-err "clip-cache-warm" e))))
   :name "paalam-clip-cache"))

(defun %clip-cache-gc ()
  "Sweep cache for (source.page) not referenced by any clip (*clips*) (#35 ③)."
  (let ((live (make-hash-table :test 'equal)))
    (dolist (c *clips*) (dolist (it (cdr c)) (setf (gethash it live) t)))
    (let (dead)
      (maphash (lambda (k v) (declare (ignore v)) (unless (gethash k live) (push k dead))) *clip-cache*)
      (dolist (k dead) (%clip-cache-drop k)))))

;;; Switch/create/rename/delete named multiple clips (#35 ③). Operated from the clip pane bar.
(defun %clip-pane-refresh-if-visible (vw)
  (when (and (viewer-clip-pane vw) (dotnet:invoke (viewer-clip-pane vw) "get_IsVisible"))
    (%rebuild-clip-pane vw)))
(defun %clip-switch (vw index)
  "Switch to the clip at INDEX (data + pane/title only). **Do not rebuild the combo's Items**:
this is called from the combo's SelectionChanged, so doing Clear+Add here re-enters and the
combo's layout (height) collapses, leaving it 'pick once and never pickable again'. Update Items on the new/rename/delete side."
  (when (and (>= index 0) (< index (length *clips*)) (/= index *current-clip*))
    (%clip-sync)
    (setf *current-clip* index *clip* (cdr (clip-current-cell))
          ;; For a new clip, reset cursor/selection/pending (index depends on contents; #35 ④)
          (viewer-clip-cursor vw) 0 (viewer-clip-selected vw) nil
          (viewer-clip-pending vw) nil (viewer-clip-count vw) nil)
    (save-clip) (%clip-cache-warm) (%update-title vw)
    (%clip-pane-refresh-if-visible vw)))
(defun %clip-new (vw name)
  "Add an empty clip named NAME and switch to it (rebuild Items)."
  (%clip-sync)
  (setf *clips* (append *clips* (list (cons name nil)))
        *current-clip* (1- (length *clips*)) *clip* (cdr (clip-current-cell)))
  (save-clip) (%clip-cache-warm) (%update-title vw)
  (%refresh-clip-selector vw) (%clip-pane-refresh-if-visible vw))
(defun %clip-rename (vw name)
  (setf (car (clip-current-cell)) name)
  (save-clip) (%update-title vw) (%refresh-clip-selector vw))
(defun %clip-delete (vw)
  "Delete the current clip (never delete the last one). Also sweep caches no longer referenced."
  (when (> (length *clips*) 1)
    (setf *clips* (loop for c in *clips* for i from 0 unless (= i *current-clip*) collect c)
          *current-clip* (max 0 (min *current-clip* (1- (length *clips*))))
          *clip* (cdr (clip-current-cell)))
    (%clip-cache-gc) (save-clip) (%clip-cache-warm) (%update-title vw)
    (%refresh-clip-selector vw) (%clip-pane-refresh-if-visible vw)))
(defun %refresh-clip-selector (vw)
  "Rebuild the clip-selection combo from *clips* and select the current one (the selection-changed handler only switches on a diff)."
  (let ((combo (viewer-clip-combo vw)))
    (when combo
      (let ((items (dotnet:invoke combo "get_Items")))
        (dotnet:invoke items "Clear")
        (dolist (c *clips*) (dotnet:invoke items "Add" (car c)))
        (dotnet:invoke combo "set_SelectedIndex" *current-clip*)))))
(defun %build-clip-bar (vw)
  "Clip-selection bar at the top of the clip pane (combo + New/Rename/Delete). Held in viewer-clip-combo."
  (flet ((sb (label thunk)
           (let ((b (dotnet:new "Avalonia.Controls.Button")))
             (dotnet:invoke b "set_Content" label)
             (dotnet:invoke b "set_Padding" (dotnet:new "Avalonia.Thickness" 6d0 2d0 6d0 2d0))
             (dotnet:invoke b "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 4d0 0d0))
             (dotnet:add-event b "Click" (lambda (s e) (declare (ignore s e)) (funcall thunk)))
             b)))
    (let ((bar   (dotnet:new "Avalonia.Controls.StackPanel"))
          (combo (dotnet:new "Avalonia.Controls.ComboBox"))
          (btns  (dotnet:new "Avalonia.Controls.StackPanel")))
      (setf (viewer-clip-combo vw) combo)
      (dotnet:invoke combo "set_HorizontalAlignment" (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Stretch"))
      (dotnet:invoke combo "set_MinWidth" 150d0)   ; min width for when Stretch doesn't apply (pane 220px)
      (dotnet:add-event combo "SelectionChanged"
        (lambda (s e) (declare (ignore s e))
          (let ((i (dotnet:invoke combo "get_SelectedIndex")))
            (when (and (>= i 0) (/= i *current-clip*)) (%clip-switch vw i)))))
      (dotnet:invoke btns "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
      (dotnet:invoke btns "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 4d0 0d0 0d0))
      (let ((c (dotnet:invoke btns "get_Children")))
        (dotnet:invoke c "Add" (sb "New" (lambda () (%prompt-text (viewer-window vw) "New clip name" ""
                                                                 (lambda (n) (%clip-new vw n))))))
        (dotnet:invoke c "Add" (sb "Rename" (lambda () (%prompt-text (viewer-window vw) "Rename clip" (clip-current-name)
                                                                   (lambda (n) (%clip-rename vw n))))))
        (dotnet:invoke c "Add" (sb "Delete" (lambda () (%clip-delete vw)))))
      (dotnet:invoke bar "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 0d0 4d0))
      (let ((c (dotnet:invoke bar "get_Children")))
        ;; Heading indicating this pane is the "clip" (#35 ③).
        (dotnet:invoke c "Add" (%s-header "Clip (temporary basket of pages → export)"))
        (dotnet:invoke c "Add" combo) (dotnet:invoke c "Add" btns))
      (%refresh-clip-selector vw)
      bar)))

(defun %sanitize (s)
  (map 'string (lambda (c) (if (or (alphanumericp c) (member c '(#\- #\_ #\.))) c #\_))
       (or s "x")))

(defun %export-resize-bytes (bytes ext)
  "If *export-width* is an integer, shrink only pages wider than it and return PNG bytes (#35 ②).
At or below original size / :original returns (values bytes ext) unchanged. On failure also returns original (don't stop the export).
Decode with Avalonia's Bitmap → CreateScaledBitmap → PNG Save (same kind of path as thumbnail #6)."
  (let ((target *export-width*))
    (if (and (integerp target) (plusp target))
        (handler-case
            (let ((ms (dotnet:new "System.IO.MemoryStream")) (bmp nil) (scaled nil))
              (unwind-protect
                   (progn
                     ;; MemoryStream(byte[]) ctor hits an overload bug (#294), so pass via Write.
                     (dotnet:invoke ms "Write" bytes 0 (dotnet:invoke bytes "get_Length"))
                     (dotnet:invoke ms "set_Position" 0)
                     (setf bmp (dotnet:new "Avalonia.Media.Imaging.Bitmap" ms))
                     (let* ((px (dotnet:invoke bmp "get_PixelSize"))
                            (w  (dotnet:invoke px "get_Width")))
                       (if (<= w target)
                           (values bytes ext)        ; pass through unchanged if at or below original size
                           (let* ((h   (dotnet:invoke px "get_Height"))
                                  (th  (max 1 (round (* h target) w)))
                                  (out (dotnet:new "System.IO.MemoryStream")))
                             (setf scaled
                                   (dotnet:invoke bmp "CreateScaledBitmap"
                                     (dotnet:new "Avalonia.PixelSize" target th)
                                     (dotnet:static "Avalonia.Media.Imaging.BitmapInterpolationMode" "HighQuality")))
                             (unwind-protect
                                  (progn (dotnet:invoke scaled "Save" out)
                                         (values (dotnet:invoke out "ToArray") "png"))
                               (dotnet:invoke out "Dispose"))))))
                (ignore-errors (when scaled (dotnet:invoke scaled "Dispose")))
                (ignore-errors (when bmp (dotnet:invoke bmp "Dispose")))
                (dotnet:invoke ms "Dispose")))
          (error (e) (log-err "export-resize" e) (values bytes ext)))
        (values bytes ext))))

(defun %write-clip-zip (out clip)
  "Write each page of CLIP to zip OUT with sequential renaming (each source is re-opened and read).
Call from a background thread so the UI doesn't freeze. Returns OUT."
  (when clip
    (ignore-errors (dotnet:load-assembly "System.IO.Compression"))
    (let* ((fs (dotnet:static "System.IO.File" "Create" out))
           (zip nil)
           (books (make-hash-table :test 'equal))
           (seq 0))
      (unwind-protect
           (progn
             (setf zip (dotnet:new "System.IO.Compression.ZipArchive" fs
                                   (dotnet:static "System.IO.Compression.ZipArchiveMode" "Create")))
             (dolist (item clip)
               (incf seq)
               (handler-case
                   (let ((src (car item)) (page (cdr item)))
                     ;; #35 ①: if a local cache exists, fill from it instead of re-reading the NAS.
                     ;; Otherwise re-open and read the source as before (compatible with existing clip.lisp).
                     (multiple-value-bind (cpath cname) (%clip-cached src page)
                       (multiple-value-bind (bytes ext name)
                           (if cpath
                               (values (dotnet:static "System.IO.File" "ReadAllBytes" cpath)
                                       (%name-ext cpath) cname)
                               (let ((book (or (gethash src books)
                                               (setf (gethash src books) (open-book src)))))
                                 (multiple-value-bind (b e) (book-page-bytes book page)
                                   (values b e (book-page-name book page)))))
                         ;; #35 ②: shrink (to PNG) pages exceeding the configured max width and fill them in.
                         (multiple-value-bind (wbytes wext) (%export-resize-bytes bytes ext)
                           (let* ((nm (format nil "~4,'0d_~a.~a" seq (%sanitize (pathname-name name)) wext))
                                  (entry (dotnet:invoke zip "CreateEntry" nm))
                                  (es (dotnet:invoke entry "Open")))
                             (unwind-protect
                                  (dotnet:invoke es "Write" wbytes 0 (dotnet:invoke wbytes "get_Length"))
                               (dotnet:invoke es "Dispose")))))))
                 (error (e) (log-err "export-clip-item" e)))))
        (when zip (ignore-errors (dotnet:invoke zip "Dispose")))
        (ignore-errors (dotnet:invoke fs "Dispose"))
        (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (book-close v))) books))
      out)))

;;; Clip pane (#18 Phase 2): show the collected pages as a thumbnail list. Thumbnails are
;;; loaded asynchronously on a background thread and inserted later (don't freeze the UI even on NAS).

(defun %clip-remove-at (vw idx)
  "Remove the clip item at idx and update the pane. Also sweep the local cache (#35 ①)."
  (let ((item (nth idx *clip*)))
    (setf *clip* (loop for e in *clip* for i from 0 unless (= i idx) collect e))
    (when item (%clip-cache-drop item)))
  (save-clip)
  (%update-title vw)
  (%rebuild-clip-pane vw))

(defun %reorder-target (vw y)
  "Compute the insertion position (0..n) from the Y coordinate within clip-stack (decide by each cell's midpoint)."
  (let* ((children (dotnet:invoke (viewer-clip-stack vw) "get_Children"))
         (n (dotnet:invoke children "get_Count")))
    (dotimes (i n n)
      (let* ((cell (dotnet:invoke children "get_Item" i))
             (b   (dotnet:invoke cell "get_Bounds"))
             (top (dotnet:invoke b "get_Y"))
             (h   (dotnet:invoke b "get_Height")))
        (when (< y (+ top (/ h 2.0d0))) (return-from %reorder-target i))))))

(defun %clip-reorder (vw from to)
  "Move the clip item at from to the insertion position to (0..n) relative to the original list, and update the pane."
  (when (and from (/= from to) (>= from 0) (< from (length *clip*)))
    (let* ((item (nth from *clip*))
           (rest (loop for e in *clip* for i from 0 unless (= i from) collect e))
           (pos  (max 0 (min (if (> to from) (1- to) to) (length rest)))))
      (setf *clip* (append (subseq rest 0 pos) (list item) (subseq rest pos)))
      (save-clip)
      (%rebuild-clip-pane vw))))

;;; Visual feedback for reorder (#20): make the grabbed thumbnail semi-transparent + a blue line at the insertion position.
(defun %ensure-reorder-line (vw)
  "Prepare the insertion-position indicator Border (reused)."
  (or (viewer-reorder-line vw)
      (setf (viewer-reorder-line vw)
            (let ((b (dotnet:new "Avalonia.Controls.Border")))
              (dotnet:invoke b "set_Height" 3d0)
              (dotnet:invoke b "set_Margin" (dotnet:new "Avalonia.Thickness" 4d0 0d0 4d0 0d0))
              (dotnet:invoke b "set_Background" (dotnet:static "Avalonia.Media.Brushes" "DeepSkyBlue"))
              b))))

(defun %show-reorder-line (vw y)
  "While dragging at Y in clip-stack, move the indicator to the planned insertion position."
  (let ((children (dotnet:invoke (viewer-clip-stack vw) "get_Children"))
        (line (%ensure-reorder-line vw)))
    ;; Remove it first, then compute the target position (so line itself doesn't shift the index).
    (ignore-errors (dotnet:invoke children "Remove" line))
    (dotnet:invoke children "Insert" (%reorder-target vw y) line)))

(defun %clear-reorder-visual (vw)
  "Remove the insertion line and restore the grabbed thumbnail's opacity (on reorder end/abort)."
  (let ((line (viewer-reorder-line vw)) (img (viewer-reorder-img vw)))
    (when line
      (ignore-errors
        (dotnet:invoke (dotnet:invoke (viewer-clip-stack vw) "get_Children") "Remove" line)))
    (when img (ignore-errors (dotnet:invoke img "set_Opacity" 1d0)))))

;;; View the whole clip as a single virtual book (#35 ⑤ preview). A book-protocol implementation
;;; that holds each (source . page) of the clip as a page. Loading prefers the local cache of ① (avoids NAS re-reads);
;;; if absent, opens the source and reads (source handles are cached lazily and guarded by a lock).
(defclass clip-book ()
  ((items   :initarg :items   :accessor cb-items)        ; vector of (source . page)
   (sources :initform (make-hash-table :test 'equal) :accessor cb-sources)  ; source -> opened book
   (lock    :initform (make-lock "clip-book") :accessor cb-lock)))
(defun make-clip-book (items) (make-instance 'clip-book :items (coerce items 'vector)))
(defmethod book-page-count ((b clip-book)) (length (cb-items b)))
(defmethod book-page-name ((b clip-book) i)
  (let ((it (aref (cb-items b) i)))
    (format nil "~a:~d" (file-namestring (car it)) (1+ (cdr it)))))
(defmethod book-parallel-safe-p ((b clip-book)) t)   ; 1 shared handle + internal lock
(defun %cb-source (b source)
  "Open (if needed) and return the book for source. Call under cb-lock."
  (or (gethash source (cb-sources b))
      (setf (gethash source (cb-sources b)) (open-book source))))
(defun %decode-bytes (bytes)
  "Decode byte[] into an Avalonia Bitmap (MemoryStream uses Write to avoid #294)."
  (let ((ms (dotnet:new "System.IO.MemoryStream")))
    (unwind-protect
         (progn (dotnet:invoke ms "Write" bytes 0 (dotnet:invoke bytes "get_Length"))
                (dotnet:invoke ms "set_Position" 0)
                (dotnet:new "Avalonia.Media.Imaging.Bitmap" ms))
      (dotnet:invoke ms "Dispose"))))
(defmethod book-load-image ((b clip-book) i)
  (let* ((it (aref (cb-items b) i))
         (cpath (%clip-cached (car it) (cdr it))))
    (if cpath
        ;; Decode from the local cache (①): no lock needed, works in parallel.
        (%decode-bytes (dotnet:static "System.IO.File" "ReadAllBytes" cpath))
        ;; Fallback: open and read the source (lock since the handle is shared).
        (with-lock-held ((cb-lock b))
          (book-load-image (%cb-source b (car it)) (cdr it))))))
(defmethod book-page-bytes ((b clip-book) i)
  (let* ((it (aref (cb-items b) i))
         (cpath (%clip-cached (car it) (cdr it))))
    (if cpath
        (values (dotnet:static "System.IO.File" "ReadAllBytes" cpath) (%name-ext cpath))
        (with-lock-held ((cb-lock b)) (book-page-bytes (%cb-source b (car it)) (cdr it))))))
(defmethod book-close ((b clip-book))
  (with-lock-held ((cb-lock b))
    (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (book-close v))) (cb-sources b))
    (clrhash (cb-sources b))))

(defun %open-clip-as-book (vw)
  "Open the entire current clip as a single book in the main viewer (#35 ⑤). Do nothing if empty."
  (let ((items (copy-list *clip*)))
    (if (null items)
        (%status vw "Clip is empty")
        (handler-case
            (let ((book (make-clip-book items))
                  (old-loader (viewer-loader vw)) (old-book (viewer-book vw)))
              (ignore-errors (loader-quiesce old-loader))
              (setf *page-direction* (%book-direction book)
                    (viewer-source vw) nil          ; no path (excluded from history/re-open)
                    (viewer-rotation vw) 0
                    (viewer-search-matches vw) nil (viewer-search-index vw) 0)
              (%slideshow-stop vw)
              (setf (viewer-book vw) book
                    (viewer-loader vw) (open-loader (lambda () (make-clip-book items))
                                                    (lambda (i) (%ui-post (lambda () (%on-page-loaded vw i)))))
                    (viewer-spreads vw) (compute-spreads (book-page-count book))
                    (viewer-sindex vw) 0)
              (%update-title vw)
              (show-spread vw)
              (%thumb-pane-refresh vw)
              (%defer-free old-loader old-book)
              (%focus-viewport vw))         ; don't leave focus on a button (it would eat Space etc.)
          (error (e) (log-err "open-clip-as-book" e))))))

;;; vi editing for the clip (#35 ④). Active only while clip-stack has focus
;;; (keys use Handled=t so they don't leak to the window's %on-key). Phase 1=cursor/move/delete/follow,
;;; Phase 2=dd/yy→register→p/P (reorder, move between clips), Space multi-select, count.
(defparameter *clip-register* nil
  "Register for cut/yank ((source . page) ...). Shared across clips → p can transplant into another clip.")

(defun %clip-cursor-highlight (vw)
  "Show cursor(blue)/selected(gold)/both(orange) via each cell's border, and scroll the cursor into view."
  (let* ((children (dotnet:invoke (viewer-clip-stack vw) "get_Children"))
         (n (dotnet:invoke children "get_Count"))
         (cur (viewer-clip-cursor vw))
         (sel (viewer-clip-selected vw)))
    (dotimes (i n)
      (ignore-errors
        (dotnet:invoke (dotnet:invoke children "get_Item" i) "set_BorderBrush"
                       (dotnet:static "Avalonia.Media.Brushes"
                                      (cond ((and (= i cur) (member i sel)) "Orange")
                                            ((= i cur) "DeepSkyBlue")
                                            ((member i sel) "Gold")
                                            (t "Transparent"))))))
    ;; Follow by moving ScrollViewer.Offset directly so the cursor doesn't go off-screen (#35 ④).
    (let ((scr (viewer-clip-scroll vw)))
      (when (and scr (plusp n) (< cur n))
        (ignore-errors
          (let* ((b   (dotnet:invoke (dotnet:invoke children "get_Item" cur) "get_Bounds"))
                 (cy  (dotnet:invoke b "get_Y"))
                 (ch  (dotnet:invoke b "get_Height"))
                 (vh  (dotnet:invoke (dotnet:invoke scr "get_Viewport") "get_Height"))
                 (off (dotnet:invoke scr "get_Offset"))
                 (oy  (dotnet:invoke off "get_Y"))
                 (ny  (cond ((< cy oy) cy)
                            ((> (+ cy ch) (+ oy vh)) (max 0d0 (- (+ cy ch) vh)))
                            (t oy))))
            (when (/= ny oy)
              (dotnet:invoke scr "set_Offset" (dotnet:new "Avalonia.Vector" 0d0 ny)))))))))

(defun %clip-cursor-move (vw d)
  (let ((n (length *clip*)))
    (when (plusp n)
      (setf (viewer-clip-cursor vw) (max 0 (min (1- n) (+ (viewer-clip-cursor vw) d))))
      (%clip-cursor-highlight vw))))

(defun %clip-item= (a b)
  (and (string-equal (car a) (car b)) (eql (cdr a) (cdr b))))
(defun %clip-target-indices (vw count)
  "Indices to operate on (ascending). The selection if any, otherwise count items from the cursor (default 1)."
  (let ((sel (viewer-clip-selected vw)))
    (if sel
        (sort (remove-duplicates (copy-list sel)) #'<)
        (let ((cur (viewer-clip-cursor vw)) (n (length *clip*)))
          (loop for i from cur below (min n (+ cur (max 1 (or count 1)))) collect i)))))
(defun %clip-cut (vw count)
  "Remove the targets into the register (cut). Keep the cache (since p may transplant them)."
  (let ((idxs (%clip-target-indices vw count)))
    (when idxs
      (setf *clip-register* (mapcar (lambda (i) (nth i *clip*)) idxs)
            ;; Note: LOOP's `it` is a reserved word for the when/unless test value, so don't use it as a variable name.
            *clip* (loop for x in *clip* for i from 0 unless (member i idxs) collect x)
            (viewer-clip-selected vw) nil
            (viewer-clip-cursor vw) (max 0 (min (first idxs) (max 0 (1- (length *clip*))))))
      (save-clip)
      (%status vw (format nil "Cut ~d items" (length idxs)))
      (%rebuild-clip-pane vw))))
(defun %clip-yank (vw count)
  "Copy the targets into the register (yank, don't remove)."
  (let ((idxs (%clip-target-indices vw count)))
    (when idxs
      (setf *clip-register* (mapcar (lambda (i) (nth i *clip*)) idxs)
            (viewer-clip-selected vw) nil)
      (%status vw (format nil "Copied ~d items" (length idxs)))
      (%clip-cursor-highlight vw))))
(defun %clip-paste (vw before)
  "Insert the register at the cursor position of the current clip (before=t for in front). Skip already-present items.
Switching to another clip and then p makes it 'move pages between clips'."
  (when *clip-register*
    (let* ((cur (viewer-clip-cursor vw))
           (pos (min (length *clip*) (if before cur (1+ cur))))
           (fresh (remove-if (lambda (it) (member it *clip* :test #'%clip-item=)) *clip-register*)))
      (when fresh
        (setf *clip* (append (subseq *clip* 0 pos) fresh (subseq *clip* pos))
              (viewer-clip-cursor vw) pos)
        (save-clip)
        (%clip-cache-warm)               ; ensure the local cache for the transplanted source(s)
        (%status vw (format nil "Pasted ~d items" (length fresh)))
        (%rebuild-clip-pane vw)))))
(defun %clip-toggle-select (vw)
  (let ((cur (viewer-clip-cursor vw)))
    (setf (viewer-clip-selected vw)
          (if (member cur (viewer-clip-selected vw))
              (remove cur (viewer-clip-selected vw))
              (cons cur (viewer-clip-selected vw))))
    (%clip-cursor-highlight vw)))

(defun %rebuild-clip-pane (vw)
  "Rebuild clip-stack. Cached thumbnails show immediately; only uncached ones are loaded in the
background and inserted while accumulating into %thumb-cache (no source re-fetch from the second time on)."
  (let* ((stack (viewer-clip-stack vw))
         (children (dotnet:invoke stack "get_Children"))
         (snapshot (copy-list *clip*))
         (pending '()))                  ; (img . (source . page)) uncached ones
    (dotnet:invoke children "Clear")
    (loop for item in snapshot for i from 0 do
      (let ((img  (dotnet:new "Avalonia.Controls.Image"))
            (cell (dotnet:new "Avalonia.Controls.Grid"))   ; overlay img + ×
            (xbtn (dotnet:new "Avalonia.Controls.Button"))
            (hl   (dotnet:new "Avalonia.Controls.Border")) ; cursor border (#35 ④)
            (idx i)
            (cached (%thumb-get (car item) (cdr item))))
        (dotnet:invoke img "set_Width" 200d0)
        (dotnet:invoke img "set_Stretch" (dotnet:static "Avalonia.Media.Stretch" "Uniform"))
        ;; Reorder by left drag (#18 Phase 3). Grab on press, insert at the release position.
        ;; While dragging, make the thumbnail semi-transparent + show a blue line at the insertion position (#20).
        (dotnet:add-event img "PointerPressed"
          (lambda (s e) (declare (ignore s))
            (dotnet:invoke e "set_Handled" t)   ; don't trigger window panning
            (when (dotnet:invoke (dotnet:invoke (dotnet:invoke e "GetCurrentPoint" img)
                                                "get_Properties") "get_IsLeftButtonPressed")
              (setf (viewer-clip-cursor vw) idx)                ; #35 ④ move cursor here
              (ignore-errors (dotnet:invoke stack "Focus"))     ; into the edit context (stack focus)
              (%clip-cursor-highlight vw)
              (setf (viewer-reorder-from vw) idx (viewer-reorder-img vw) img)
              (dotnet:invoke img "set_Opacity" 0.4d0)   ; sense of grabbing
              (%ensure-reorder-line vw)
              (ignore-errors (dotnet:invoke (dotnet:invoke e "get_Pointer") "Capture" img)))))
        (dotnet:add-event img "PointerMoved"
          (lambda (s e) (declare (ignore s))
            (when (viewer-reorder-from vw)
              (%show-reorder-line vw
                (dotnet:invoke (dotnet:invoke e "GetPosition" (viewer-clip-stack vw)) "get_Y")))))
        (dotnet:add-event img "PointerReleased"
          (lambda (s e) (declare (ignore s))
            (dotnet:invoke e "set_Handled" t)
            (let ((from (viewer-reorder-from vw)))
              (when from
                (let ((y (dotnet:invoke (dotnet:invoke e "GetPosition" (viewer-clip-stack vw)) "get_Y")))
                  (%clear-reorder-visual vw)   ; remove the line before computing target (prevent index shift)
                  (let ((target (%reorder-target vw y)))
                    (setf (viewer-reorder-from vw) nil (viewer-reorder-img vw) nil)
                    (ignore-errors (dotnet:invoke (dotnet:invoke e "get_Pointer") "Capture" nil))
                    (%clip-reorder vw from target)))))))
        ;; × button (top-right). Make it stand out with red background + bold white so it reads as delete.
        (dotnet:invoke xbtn "set_Content" "×")
        (dotnet:invoke xbtn "set_Foreground" (dotnet:static "Avalonia.Media.Brushes" "White"))
        (dotnet:invoke xbtn "set_Background" (dotnet:static "Avalonia.Media.Brushes" "Firebrick"))
        (dotnet:invoke xbtn "set_FontWeight" (dotnet:static "Avalonia.Media.FontWeight" "Bold"))
        (dotnet:invoke xbtn "set_FontSize" 14d0)
        (dotnet:invoke xbtn "set_HorizontalAlignment"
                       (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Right"))
        (dotnet:invoke xbtn "set_VerticalAlignment"
                       (dotnet:static "Avalonia.Layout.VerticalAlignment" "Top"))
        (dotnet:invoke xbtn "set_Padding" (dotnet:new "Avalonia.Thickness" 7d0 2d0))
        (dotnet:add-event xbtn "Click"
                          (lambda (s e) (declare (ignore s e)) (%clip-remove-at vw idx)))
        (dotnet:invoke cell "set_Margin" (dotnet:new "Avalonia.Thickness" 4d0))
        (let ((cc (dotnet:invoke cell "get_Children")))
          (dotnet:invoke cc "Add" img)
          (dotnet:invoke cc "Add" xbtn))
        (if cached
            (dotnet:invoke img "set_Source" cached)
            (push (cons img item) pending))
        (dotnet:invoke hl "set_BorderThickness" (dotnet:new "Avalonia.Thickness" 2d0))  ; #35 ④ cursor border
        (dotnet:invoke hl "set_Child" cell)
        (dotnet:invoke children "Add" hl)))
    ;; Clamp the cursor into range and apply the border highlight (#35 ④).
    (let ((n (length snapshot)))
      (setf (viewer-clip-cursor vw) (max 0 (min (viewer-clip-cursor vw) (max 0 (1- n))))))
    (%clip-cursor-highlight vw)
    (when pending
      (setf pending (nreverse pending))
      (make-thread
       (lambda ()
         (let ((books (make-hash-table :test 'equal)))
           (unwind-protect
                (dolist (pair pending)
                  (ignore-errors
                    (let* ((img (car pair)) (item (cdr pair))
                           (src (car item)) (page (cdr item))
                           (book (or (gethash src books)
                                     (setf (gethash src books) (open-book src))))
                           (bmp (book-load-image book page)))
                      (when bmp
                        (%thumb-put src page bmp)
                        (%ui-post (lambda () (dotnet:invoke img "set_Source" bmp)))))))
             (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (book-close v))) books))))
       :name "paalam-thumbs"))))

(defun %do-export (vw out)
  "Write the clip to out on a background thread, then update the UI when done (clear the clip)."
  (let ((snapshot (copy-list *clip*)))
    (make-thread
     (lambda ()
       (ignore-errors (%write-clip-zip out snapshot))
       (%ui-post (lambda ()
                   (clip-clear) (%update-title vw) (%rebuild-clip-pane vw)
                   (dotnet:invoke (viewer-window vw) "set_Title"
                                  (format nil "Export complete → ~a" out)))))
     :name "paalam-export")))

(defun %pane-export (vw)
  "Pick a destination via the save dialog and export to zip in the background."
  (when *clip*
    (let ((suggested (concatenate 'string "paalam-clip-"
                                  (dotnet:invoke (dotnet:static "System.DateTime" "Now")
                                                 "ToString" "yyyyMMdd-HHmmss")
                                  ".zip")))
      (dotnet:static "Paalam.DialogHelper" "PickSaveZip" (viewer-window vw) suggested
                     (dotnet:make-delegate "System.Action`1[System.String]"
                                           (lambda (out) (%do-export vw out)))))))

(defun %close-clip-pane (vw)
  "Hide the clip pane and return focus to the view (#38). Used by the clip 'q' key and as the
close half of the toggle. Esc inside the clip only parks focus on the view (pane stays open);
this actually closes it."
  (let ((pane (viewer-clip-pane vw)))
    (when (dotnet:invoke pane "get_IsVisible")
      (dotnet:invoke pane "set_IsVisible" nil)
      (%focus-viewport vw))))

(defun %toggle-clip-pane (vw)
  ;; #35 ④: P = toggle. On open, focus the clip so you can edit right away; on close, focus the view.
  ;; Inside the clip, Esc just peeks at the view (pane stays open); q (:clip-close) actually closes (#38).
  (let* ((pane (viewer-clip-pane vw))
         (vis  (dotnet:invoke pane "get_IsVisible")))
    (if vis
        (%close-clip-pane vw)              ; open → close (hide + focus view)
        (progn                             ; closed → open: show, rebuild, focus the clip
          (dotnet:invoke pane "set_IsVisible" t)
          (%rebuild-clip-pane vw)
          (%defer (lambda ()
                    (ignore-errors (dotnet:invoke (viewer-clip-stack vw) "Focus"))
                    (%clip-cursor-highlight vw))
                  120)))))

(defun %clip-drop (vw path)
  "Add the path dropped onto the pane to the clip (image=parent folder + that page;
folder/archive=all pages)."
  (handler-case
      (if (and (%file-exists path)
               (image-ext-p (pathname-type path)))
          ;; Image: parent folder (with drive) as source, its index within as page.
          ;; directory-namestring drops the drive, so use Path.GetDirectoryName.
          (let* ((dir (substitute #\/ #\\
                                  (dotnet:static "System.IO.Path" "GetDirectoryName" path)))
                 (files (%dir-image-files dir))
                 (idx (or (position (file-namestring path) files
                                    :key #'file-namestring :test #'string-equal)
                          0)))
            (clip-add dir idx))
          ;; Folder/archive: all pages.
          (multiple-value-bind (book start) (open-book path)
            (declare (ignore start))
            (unwind-protect
                 (dotimes (p (book-page-count book)) (clip-add path p))
              (ignore-errors (book-close book)))))
    (error (e) (log-err "clip-drop" e)))
  (%clip-cache-warm)                   ; #35 ①: copy the added items to local in the background
  (%update-title vw)
  (when (dotnet:invoke (viewer-clip-pane vw) "get_IsVisible") (%rebuild-clip-pane vw)))

