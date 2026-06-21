;;;; settings.lisp — paalam (#39 main.lisp split): display settings / persist / history / bookmarks
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; Display settings / persist / history / bookmarks (viewer body in viewer.lisp)
;;; ------------------------------------------------------------------

;;; --- Display settings (target of future customization UI / settings file) ---------
(defparameter *page-direction* :rtl
  "Current book's binding direction (runtime value). :rtl=right-opening/manga (<- is next, -> is prev), :ltr=left-opening.
When a book is opened it is decided by its type from *image-direction* / *pdf-direction*, and the D key flips it in place (#33).")
(defparameter *image-direction* :rtl
  "Default binding direction when opening an image/archive (manga) (#33).")
(defparameter *pdf-direction* :ltr
  "Default binding direction when opening a PDF (paper/technical book) (#33).")
(defun %book-direction (book)
  "Default binding direction per book type (#33). pdf=*pdf-direction* / image,archive=*image-direction*."
  (if (typep book 'pdf-book) *pdf-direction* *image-direction*))
(defparameter *spread* t
  "Spread display. t = lay out 2 pages, nil = single page.")
(defparameter *first-page-single* t
  "In spread mode, show the first page (cover) alone (manga convention).")
(defparameter *recursive* t
  "When opening a folder as a book, recursively gather images from subfolders too.")
(defparameter *fit-mode* :whole
  "Fit mode. :whole=whole(centered), :width=fit width(vertical scroll),
:height=fit height(horizontal scroll). Effective scale = fit baseline x user zoom (#15).")
(defparameter *background-color* "Black"
  "Brush name for the window background (= margin/letterbox when fitting). A name from Avalonia.Media.Brushes (#2).")
(defparameter *interpolation* :high
  "Interpolation quality when scaling up. :high=HighQuality (smooth), :low=LowQuality (sharper) (#2).")
(defparameter *theme* :system
  "UI theme (color scheme of the FluentTheme parts such as menu/dialog). :system=follow the OS light/dark
(ThemeVariant.Default), :light / :dark to fix (#27). The image display background is separate: *background-color*.")

;;; --- Persisting settings (s-expr plist / OS app data) ---------------------
(defparameter *export-width* :original
  "Max width on export (#35 ②). :original=original size (no conversion) / integer=only pages exceeding
that width are shrunk and output as PNG. Pages at or below original size pass through unconverted (no upscaling, no re-encoding).")

(defparameter *settings*
  '((:image-direction   . *image-direction*)
    (:pdf-direction     . *pdf-direction*)
    (:export-width      . *export-width*)
    (:spread            . *spread*)
    (:first-page-single . *first-page-single*)
    (:recursive         . *recursive*)
    (:fit-mode          . *fit-mode*)
    (:background-color  . *background-color*)
    (:interpolation     . *interpolation*)
    (:theme             . *theme*)
    (:pdf-dpi           . *pdf-dpi*)
    (:cache-memory-mb   . *cache-memory-mb*)
    (:cache-size        . *cache-size*)
    (:prefetch-ahead    . *prefetch-ahead*)
    (:slideshow-interval . *slideshow-interval*)
    (:loupe-scale       . *loupe-scale*)
    (:loader-threads    . *loader-threads*))
  "(settings key . special-variable symbol) of save targets. Just adding here makes it a persistence target.")

(defun %config-dir ()
  "per-user settings directory + paalam/.
Win=%APPDATA%, Linux=~/.config (XDG): GetFolderPath(ApplicationData) returns them correctly.
Only on macOS, GetFolderPath has a .NET quirk of returning ~/.config, so
steer to the Mac-native ~/Library/Application Support (dotnet/runtime #40353)."
  (let ((base
          (if (dotnet:static "System.OperatingSystem" "IsMacOS")
              (concatenate 'string
                           (dotnet:static "System.Environment" "GetFolderPath"
                                          (dotnet:static "System.Environment+SpecialFolder" "UserProfile"))
                           "/Library/Application Support")
              (dotnet:static "System.Environment" "GetFolderPath"
                             (dotnet:static "System.Environment+SpecialFolder" "ApplicationData")))))
    (concatenate 'string base "/paalam/")))

(defun %config-file () (concatenate 'string (%config-dir) "settings.lisp"))

(defun save-settings ()
  "Write out the current settings values as an s-expr plist."
  (handler-case
      (progn
        (dotnet:static "System.IO.Directory" "CreateDirectory" (%config-dir))
        (with-open-file (s (%config-file) :direction :output
                           :if-exists :supersede :if-does-not-exist :create)
          (let ((plist '()))
            (dolist (e *settings*)
              (setf plist (list* (car e) (symbol-value (cdr e)) plist)))
            (with-standard-io-syntax (prin1 plist s) (terpri s)))))
    (error (e) (log-err "save-settings" e))))

(defun load-settings ()
  "If a settings file exists, load and apply it. If missing/corrupt, recreate with default values."
  (let ((file (%config-file)))
    (handler-case
        (if (dotnet:static "System.IO.File" "Exists" file)
            (with-open-file (s file :direction :input)
              (let* ((*read-eval* nil) (plist (read s nil nil)))
                (when (consp plist)
                  (loop for (k v) on plist by #'cddr
                        for e = (assoc k *settings*)
                        when e do (setf (symbol-value (cdr e)) v)))))
            (save-settings))
      (error (e)
        (log-err "load-settings" e)
        (ignore-errors (save-settings))))))   ; if corrupt, recreate with defaults

;;; --- History + bookmarks (#7) -------------------------------------
;;; History: remember the last page viewed per book, and resume from there when reopened.
;;; Bookmarks: bookmark a page (B toggle, ★ display, [ ] jump to prev/next bookmark).
;;; Both persist to app data as s-exprs. The cross-cutting browse UI is #6/#9.

(defun %state-file (name) (concatenate 'string (%config-dir) name))

(defun %read-sexp-file (file)
  "If FILE exists, read and return one s-expr (*read-eval* nil). nil if missing/corrupt.
Arbitrary-path capable (common base for settings import and state-file reads; #24/#34)."
  (handler-case
      (when (dotnet:static "System.IO.File" "Exists" file)
        (with-open-file (s file :direction :input)
          (let ((*read-eval* nil)) (read s nil nil))))
    (error (e) (log-err (format nil "read ~a" file) e) nil)))

(defun %atomic-write (file data)
  ;; Atomic write (#34): write to temp and replace via rename. Even with multiple
  ;; instances running at once, a reader never sees a half-written intermediate file (= prevents
  ;; parse failure from a partial read -> overwrite with defaults). Put the PID in the temp name so it
  ;; never collides with another instance's temp. File.Move(overwrite) within the same directory is effectively an atomic rename.
  ;; Arbitrary-path capable (common base for settings export and state-file writes; #24/#34).
  (handler-case
      (let* ((dir (dotnet:static "System.IO.Path" "GetDirectoryName" file))
             (tmp (concatenate 'string file "."
                               (princ-to-string (dotnet:static "System.Environment" "ProcessId"))
                               ".tmp")))
        (when (and dir (plusp (length dir)))
          (dotnet:static "System.IO.Directory" "CreateDirectory" dir))
        (unwind-protect
             (progn
               (with-open-file (s tmp :direction :output
                                  :if-exists :supersede :if-does-not-exist :create)
                 (with-standard-io-syntax (prin1 data s) (terpri s)))
               (dotnet:static "System.IO.File" "Move" tmp file t))   ; overwrite=t
          (when (dotnet:static "System.IO.File" "Exists" tmp)
            (ignore-errors (dotnet:static "System.IO.File" "Delete" tmp)))))
    (error (e) (log-err (format nil "write ~a" file) e))))

(defun %read-state (name) (%read-sexp-file (%state-file name)))
(defun %write-state (name data) (%atomic-write (%state-file name) data))

;;; History
(defparameter *history* nil "((path . page) ...) most-recent first.")
(defparameter *history-limit* 200)
(defun load-history () (setf *history* (%read-state "history.lisp")))
(defun %merge-history (mine disk)
  "Merge giving priority to one's own history while keeping paths that exist only on disk (#34). So that
even when multiple instances open/close different books, neither side's history is wiped out by last-writer-wins. Paths are
deduplicated with string-equal semantics (case-insensitive). Keep newest-first order and truncate at *history-limit*."
  (let ((seen (make-hash-table :test 'equal)) (out '()))
    (dolist (e (append mine disk))
      (let ((k (string-downcase (car e))))
        (unless (gethash k seen)
          (setf (gethash k seen) t)
          (push e out))))
    (setf out (nreverse out))
    (if (> (length out) *history-limit*) (subseq out 0 *history-limit*) out)))
(defun save-history ()
  ;; Merge with the current disk state right before writing (preserve other instances' history; #34).
  (setf *history* (%merge-history *history* (%read-state "history.lisp")))
  (%write-state "history.lisp" *history*))
(defun history-page (path) (cdr (assoc path *history* :test #'string-equal)))
(defun history-update (path page)
  (when path
    (setf *history* (cons (cons path page)
                          (remove path *history* :key #'car :test #'string-equal)))
    (when (> (length *history*) *history-limit*)
      (setf *history* (subseq *history* 0 *history-limit*)))))

;;; Bookmarks
(defparameter *bookmarks* nil "((path . page) ...).")
(defun load-bookmarks () (setf *bookmarks* (%read-state "bookmarks.lisp")))
(defun save-bookmarks () (%write-state "bookmarks.lisp" *bookmarks*))
(defun bookmarked-p (path page)
  (and path (member-if (lambda (e) (and (string-equal (car e) path) (eql (cdr e) page)))
                       *bookmarks*)))
(defun toggle-bookmark (path page)
  (when path
    (if (bookmarked-p path page)
        (setf *bookmarks* (remove-if (lambda (e) (and (string-equal (car e) path)
                                                      (eql (cdr e) page)))
                                     *bookmarks*))
        (push (cons path page) *bookmarks*))
    (save-bookmarks)))
(defun book-bookmark-pages (path)
  (sort (loop for e in *bookmarks* when (string-equal (car e) path) collect (cdr e)) #'<))

