;;;; Build-time: src/main.lisp -> paalam.fasl (単一 fasl, #22 Phase 2)。
;;;; define-class 等のマクロを使うので依存を先に require してから compile-file。
(require :dotnet-class)
(require :dotcl-thread)
(handler-case
    (progn
      (compile-file "src/main.lisp" :output-file "paalam.fasl")
      (format t "COMPILE-OK: paalam.fasl~%"))
  (error (e) (format t "COMPILE-FAIL: ~a~%" e)))
