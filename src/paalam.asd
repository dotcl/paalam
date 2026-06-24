;;;; paalam system definition (#22 distribution scaffolding / #39 file split).
;;;; :components is load order (earlier macros / special variables affect later compilation).
;;;; Runtime external deps are :depends-on. The require of dotnet-class / dotcl-thread is done
;;;; in package.lisp's eval-when (works around concatenate-source-op's single-unit compile).
(defsystem "paalam"
  :description "A NeeView-style image viewer built with dotcl + Avalonia"
  :author "SANO Masatoshi <snmsts@gmail.com>"
  :maintainer "SANO Masatoshi <snmsts@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on ("dotnet-class" "dotcl-thread")
  :components ((:file "package")
               (:file "util")
               (:file "pdf-ffi")
               (:file "book")
               (:file "settings")
               (:file "clip")
               (:file "dnd")
               (:file "thumb-pane")
               (:file "browse-pane")
               (:file "loader")
               (:file "viewer")
               (:file "info-pane")
               (:file "command")
               (:file "window")
               (:file "palette")
               (:file "config-io")
               (:file "menu")
               (:file "editor")
               (:file "app")))
