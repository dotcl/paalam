;;;; package.lisp — paalam パッケージ定義 + contrib require (#39 main.lisp 分割の先頭)
;;;;
;;;; GUI は Avalonia (OSS, MIT, クロスプラット)。dotcl host (paalam.exe) が
;;;; DotclHost.Call("RUN-PAALAM" target) を呼び、Lisp 側で AppBuilder を起動する。
;;;; net10.0 (-windows 不要)・STA 不要・Windows Desktop ランタイム非依存。
;;;;
;;;; 「本 (book)」= ページの供給源 (ディレクトリ / アーカイブ) を抽象化した
;;;; CLOS プロトコル。directory-book / zip-book が実装する。GUI 非依存。

(defpackage :paalam
  (:use :cl)
  (:export :run-paalam :build-main-window :open-book
           :book-page-count :book-page-name :book-load-image :book-close))
(in-package :paalam)

;;; define-class (UI クラス emit) とスレッド/ロック/条件変数 (先読みローダー) の contrib。
;;; DotCL.Runtime 0.1.11+ は contrib を出力に同梱するので require で解決できる。
;;; *package* = :paalam で require することで、define-class マクロが束縛する `self' が
;;; この下の define-class 本体 (:paalam) と同一シンボルになる。
;;;
;;; #39: 複数 :components に分割すると concatenate-source-op が全ファイルを連結して
;;; 1 単位でコンパイルする。後続ファイルの dotnet:define-class (マクロ) が compile-time に
;;; 効くよう、マクロ/パッケージ提供の require は eval-when で compile-toplevel にも効かせる。
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :dotnet-class)
  (require :dotcl-thread)
  (use-package :dotcl-thread))

(defvar *debug* nil "PAALAM_DEBUG で有効化。先読みの詳細を debug.log に出す。")
(defun %dbg (fmt &rest args)
  (when *debug*
    (ignore-errors
      (with-open-file (s (concatenate 'string (%config-dir) "debug.log")
                         :direction :output :if-exists :append :if-does-not-exist :create)
        (format s "~a~%" (apply #'format nil fmt args))))))

(defun log-err (where e)
  ;; 署名済み .app バンドル内 (BaseDirectory) は書けないので、ホーム配下に出す。
  ;; Finder 起動時の初期化例外 (Avalonia 等) を確実に残すため。
  (ignore-errors
    (with-open-file (s (concatenate 'string
                                    (dotnet:static "System.Environment" "GetFolderPath"
                                                   (dotnet:static "System.Environment+SpecialFolder" "UserProfile"))
                                    "/paalam-error.log")
                       :direction :output :if-exists :append :if-does-not-exist :create)
      (format s "[~a] ~a~%" where e))))
