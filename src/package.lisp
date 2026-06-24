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
