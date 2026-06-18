;;;; paalam システム定義 (#22 配布の足場)。
;;;; ファイル分割の余地はここ (:components に足す)。実行時の依存は :depends-on で解決。
(defsystem "paalam"
  :description "dotcl + Avalonia による NeeView 風画像ビューア"
  :version "0.1"
  :depends-on ("dotnet-class" "dotcl-thread")
  :components ((:file "main")))
