;;;; package.lisp — paalam package definition + contrib require (#39 head of main.lisp split)
;;;;
;;;; The GUI uses Avalonia (OSS, MIT, cross-platform). The dotcl host (paalam.exe)
;;;; calls DotclHost.Call("RUN-PAALAM" target), and the Lisp side launches the AppBuilder.
;;;; net10.0 (no -windows needed), no STA required, no dependency on the Windows Desktop runtime.
;;;;
;;;; A "book" = a CLOS protocol that abstracts the page source (directory / archive).
;;;; directory-book / zip-book implement it. GUI-independent.

(defpackage :paalam
  (:use :cl)
  (:export :run-paalam :build-main-window :open-book
           :book-page-count :book-page-name :book-load-image :book-close))
(in-package :paalam)

;;; contrib for define-class (emits UI classes) and threads/locks/condition variables (prefetch loader).
;;; DotCL.Runtime 0.1.11+ bundles contrib in the output, so it can be resolved via require.
;;; Requiring with *package* = :paalam makes the `self' bound by the define-class macro
;;; the same symbol as in the define-class bodies below (:paalam).
;;;
;;; #39: When split into multiple :components, concatenate-source-op concatenates all files
;;; and compiles them as one unit. So that dotnet:define-class (a macro) in later files takes
;;; effect at compile-time, the macro/package require is also applied at compile-toplevel via eval-when.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :dotnet-class)
  (require :dotcl-thread)
  (use-package :dotcl-thread))

(defvar *debug* nil "Enabled via PAALAM_DEBUG. Writes prefetch details to debug.log.")
(defun %dbg (fmt &rest args)
  (when *debug*
    (ignore-errors
      (with-open-file (s (concatenate 'string (%config-dir) "debug.log")
                         :direction :output :if-exists :append :if-does-not-exist :create)
        (format s "~a~%" (apply #'format nil fmt args))))))

(defun log-err (where e)
  ;; A signed .app bundle (BaseDirectory) is not writable, so write under the home directory.
  ;; This reliably preserves initialization exceptions (Avalonia, etc.) when launched from Finder.
  (ignore-errors
    (with-open-file (s (concatenate 'string
                                    (dotnet:static "System.Environment" "GetFolderPath"
                                                   (dotnet:static "System.Environment+SpecialFolder" "UserProfile"))
                                    "/paalam-error.log")
                       :direction :output :if-exists :append :if-does-not-exist :create)
      (format s "[~a] ~a~%" where e))))
