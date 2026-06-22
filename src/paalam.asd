;;;; paalam システム定義 (#22 配布の足場 / #39 ファイル分割)。
;;;; :components はロード順 (前方のマクロ・特殊変数が後続のコンパイルに効く)。
;;;; 実行時の外部依存は :depends-on。dotnet-class / dotcl-thread の require は
;;;; package.lisp の eval-when で行う (concatenate-source-op の連結コンパイル対策)。
(defsystem "paalam"
  :description "dotcl + Avalonia による NeeView 風画像ビューア"
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
