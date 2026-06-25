;;;; loader.lisp — paalam (#39 main.lisp split): prefetch loader (#17)
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; prefetch loader (#17): a single worker thread handles all archive
;;; access and caches decoded Bitmaps. The UI just reads the cache. Load
;;; completion is notified to the UI via Dispatcher.UIThread.Post for swap-in.
;;; Even archives on a NAS are comfortable since neighboring pages are
;;; prefetched in the background before you turn the page.
;;; ------------------------------------------------------------------

(defparameter *cache-memory-mb* 256
  "Memory limit (MB) for the decoded page cache. This is the main budget (#25). Estimated
 by each page's actual bytes (PixelSize×4); when exceeded, Dispose from the current page in
 reading-direction backward -> farthest order.")
(defparameter *cache-size* 48
  "Maximum number of entries kept in the cache (a secondary limit; the main one is the
 memory budget *cache-memory-mb*). A guard to prevent many small images from piling up.")
(defparameter *prefetch-ahead* 8
  "How many spreads ahead of the current spread to prefetch. Backward, prefetch just a few
 min(2, this) spreads (bidirectional, forward-priority; #25). Adjustable in the settings file.")
(defparameter *slideshow-interval* 4
  "Slideshow auto-advance interval (seconds; #28).")
(defparameter *loupe-scale* 2.5d0
  "Loupe (magnifier) scale factor (#28).")
(defparameter *loader-threads* 4
  "Number of parallel loader threads. Each thread reads concurrently with its own archive
handle, hiding NAS bandwidth/latency (adjustable in the settings file).")

;; Snapshot of factory setting values (taken now that all setting variables are defined,
;; before load-settings). For the per-page \"Restore defaults\" in the settings window (#27).
;; *settings* key -> default value.
(defparameter *setting-defaults*
  (mapcar (lambda (e) (cons (car e) (symbol-value (cdr e)))) *settings*)
  "Factory (setting-key . default-value). Referenced by each page's \"Restore defaults\".")

(defstruct loader
  books                               ; vector of archive handles (shared among workers)
  (cache (make-hash-table))           ; index -> Avalonia Bitmap
  (inflight (make-hash-table))        ; index currently loading (prevents double reads)
  (lock (make-lock "paalam-loader"))
  (cv (make-condition-variable))
  (queue '())                         ; index awaiting load (in priority order)
  (current 0)                         ; LRU center (current page index)
  (pinned '())                        ; pages of the on-screen spread — never evicted, even over budget
  (direction 1)                       ; reading direction (+1=index up / -1=down; eviction bias; #25)
  (done nil)
  workers                             ; list of worker threads
  on-loaded)                          ; (lambda (index)) — notify the UI

(defun loader-get (ld index)
  (with-lock-held ((loader-lock ld)) (gethash index (loader-cache ld))))

(defun %bmp-bytes (bmp)
  "Approximate decoded byte count of a Bitmap (PixelSize×4ch). 0 if unavailable."
  (or (ignore-errors
        (let ((ps (dotnet:invoke bmp "get_PixelSize")))
          (* (dotnet:invoke ps "get_Width") (dotnet:invoke ps "get_Height") 4)))
      0))

(defun %evict-score (k cur dir)
  "Eviction priority. Larger is evicted first. The backward side of the reading direction (dir)
is evicted first; within the same direction, farthest first. The current page (s=0) is the
minimum = kept until last (#25)."
  (let ((s (* (- k cur) (if (minusp dir) -1 1))))   ; s>=0: ahead in reading direction / s<0: backward
    (if (>= s 0) s (+ 1000000 (- s)))))              ; backward is greater than any forward

(defun %loader-evict (ld)
  "When the memory budget (*cache-memory-mb*) and the count limit (*cache-size*) are exceeded,
Dispose from the current page in reading-direction backward -> farthest order (direction-aware;
#25). Avalonia Bitmaps hold Skia unmanaged memory, so always Dispose (otherwise they run out
of breath within tens to a hundred pages). The current spread's Bitmap has the minimum score,
so it is kept until last."
  (let* ((cache (loader-cache ld)) (cur (loader-current ld)) (dir (loader-direction ld))
         (budget (* *cache-memory-mb* 1024 1024))
         (total 0))
    (maphash (lambda (k v) (declare (ignore k)) (incf total (%bmp-bytes v))) cache)
    (loop while (and (> (hash-table-count cache) 1)        ; keep at least 1
                     (or (> total budget) (> (hash-table-count cache) *cache-size*)))
          do (let ((victim nil) (vscore -1) (pinned (loader-pinned ld)))
               (maphash (lambda (k v) (declare (ignore v))
                          (unless (member k pinned)            ; never evict the on-screen spread
                            (let ((sc (%evict-score k cur dir)))
                              (when (> sc vscore) (setf vscore sc victim k)))))
                        cache)
               (when (null victim) (return))                  ; only pinned remain → stop (keep them)
               (let ((bmp (gethash victim cache)))
                 (decf total (%bmp-bytes bmp))
                 (remhash victim cache)
                 (ignore-errors (dotnet:invoke bmp "Dispose")))))))

(defun loader-request (ld indices current &optional (pinned '()))
  "Request loading of indices in priority order (already-cached are skipped). current is the LRU
center. PINNED are pages of the on-screen spread that eviction must keep even when over budget."
  (with-lock-held ((loader-lock ld))
    (setf (loader-pinned ld) pinned)
    ;; Infer reading direction from the change in current (for eviction bias; keep previous direction if 0).
    (let ((prev (loader-current ld)))
      (setf (loader-direction ld) (cond ((> current prev) 1) ((< current prev) -1)
                                        (t (loader-direction ld)))))
    (setf (loader-current ld) current
          ;; Don't request what is already cached or loading (prevents double reads).
          ;; The queue is replaced every time = stale prefetch requests are naturally dropped during fast paging (#25 dedup).
          (loader-queue ld) (remove-if (lambda (i) (or (gethash i (loader-cache ld))
                                                       (gethash i (loader-inflight ld))))
                                       indices))
    (%dbg "req cur=~a want=~a queued=~a cache=~a" current (length indices)
          (length (loader-queue ld)) (hash-table-count (loader-cache ld)))
    (condition-broadcast (loader-cv ld))))   ; wake multiple workers

;;; Whether a book is safe for concurrent loading. A directory is safe since file reads are
;;; independent (one handle can be shared). zip/sharp/pdf have non-concurrent archive/native
;;; handles, so give each worker its own handle.
(defgeneric book-parallel-safe-p (book))
(defmethod book-parallel-safe-p ((b t)) nil)
(defmethod book-parallel-safe-p ((b directory-book)) t)

(defun %loader-loop (ld book)
  "Worker body. BOOK is this worker's dedicated archive handle (for parallel reads)."
  (loop
    (let ((index nil))
      (with-lock-held ((loader-lock ld))
        (loop while (and (null (loader-queue ld)) (not (loader-done ld)))
              do (condition-wait (loader-cv ld) (loader-lock ld)))
        ;; If done, drop the wait queue (prefetch) and exit immediately.
        (when (loader-done ld) (return))
        (setf index (pop (loader-queue ld)))
        (when index (setf (gethash index (loader-inflight ld)) t)))   ; mark as loading
      (when index
        ;; Never let a worker die. in-flight is always cleared.
        (unwind-protect
            (handler-case
                (let ((bmp (book-load-image book index)))   ; read with own handle (parallel)
                  (if bmp
                      (progn
                        (with-lock-held ((loader-lock ld))
                          (setf (gethash index (loader-cache ld)) bmp)
                          (%loader-evict ld))
                        (%dbg "cached ~a (cache=~a)" index (hash-table-count (loader-cache ld)))
                        (ignore-errors (funcall (loader-on-loaded ld) index)))
                      (%dbg "LOAD-NIL ~a" index)))
              (error (e) (%dbg "LOAD-ERR ~a : ~a" index e)))
          (with-lock-held ((loader-lock ld)) (remhash index (loader-inflight ld))))))))

(defun open-loader (book-factory on-loaded)
  "Loads BOOK-FACTORY (returns a new book handle with 0 args) in parallel across *loader-threads*
threads. A parallel-safe book shares one handle; otherwise a separate handle is opened per thread
(takes a factory rather than a path so clip-book can ride on it too in #35 ⑤)."
  (let* ((n (max 1 *loader-threads*))
         (first (funcall book-factory))
         (nh (if (book-parallel-safe-p first) 1 n))   ; number of handles
         (books (coerce (cons first (loop repeat (1- nh) collect (funcall book-factory))) 'vector))
         (ld (make-loader :books books :on-loaded on-loaded)))
    (setf (loader-workers ld)
          (loop for i from 0 below n
                collect (let ((bk (aref books (mod i nh))))
                          (make-thread (lambda () (%loader-loop ld bk)) :name "paalam-loader"))))
    ld))

(defun loader-quiesce (ld)
  "Stop the workers and join them (does not free the cache's Bitmaps / archive handles)."
  (with-lock-held ((loader-lock ld))
    (setf (loader-done ld) t)
    (condition-broadcast (loader-cv ld)))
  (dolist (w (loader-workers ld)) (ignore-errors (thread-join w))))   ; wait for all workers to finish

(defun loader-free (ld)
  "Free the cache's Bitmaps and archive handles (assumes already quiesced). Since it discards
the Bitmap being displayed, on book switch call it after the rendering thread (compositor) has
finished touching it = after the next render (%ui-post)."
  (with-lock-held ((loader-lock ld))
    (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (dotnet:invoke v "Dispose")))
             (loader-cache ld))
    (clrhash (loader-cache ld)))
  (loop for b across (loader-books ld) do (ignore-errors (book-close b))))   ; release handles

(defun loader-stop (ld)
  "Stop all workers and free the cache's Bitmaps and archive handles (immediate; for shutdown)."
  (loader-quiesce ld)
  (loader-free ld))

(defun %defer (thunk &optional (ms 250))
  "Run THUNK exactly once on the UI thread after MS. To give the compositor (rendering on another
thread) time to finish touching the Bitmap that was displayed before discarding it (the next tick
of %ui-post cannot guarantee rendering is complete)."
  (let ((timer (dotnet:new "Avalonia.Threading.DispatcherTimer")))
    (dotnet:invoke timer "set_Interval"
                   (dotnet:static "System.TimeSpan" "FromMilliseconds" (coerce ms 'double-float)))
    (dotnet:add-event timer "Tick"
      (lambda (s e) (declare (ignore s e))
        (dotnet:invoke timer "Stop")
        (ignore-errors (funcall thunk))))
    (dotnet:invoke timer "Start")))

(defun %defer-free (loader book)
  "Defer freeing the old loader's Bitmaps/handles and the old book until after rendering to avoid use-after-dispose."
  (%defer (lambda ()
            (ignore-errors (loader-free loader))
            (ignore-errors (book-close book)))))

(defun %ui-post (thunk)
  "Run thunk on the UI thread (worker -> UI marshaling)."
  (let ((act (dotnet:make-delegate "System.Action" thunk)))
    (dotnet:invoke (dotnet:static "Avalonia.Threading.Dispatcher" "UIThread")
                   "Post" act
                   (dotnet:static "Avalonia.Threading.DispatcherPriority" "Normal"))))

;;; viewer: book + spread units (spreads) + current position + 2 Images
;;; spreads = vector of page-index lists (1 or 2 each) per spread.
;;; single-page/spread are handled by the same mechanism (in single-page mode every element has 1).
;;; zoom/tx/ty + scale-xf/trans-xf are the stage's RenderTransform (zoom/pan).
;;; loader = prefetch loader.
