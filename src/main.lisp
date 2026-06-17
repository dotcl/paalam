;;;; main.lisp — paalam 画像ビューア本体 (定義のみ; 副作用なし)
;;;;
;;;; 2 通りの起動経路から使われる:
;;;;  - 配布: paalam.exe (WPF host) が DotclHost.Call("BUILD-APP") を呼ぶ。
;;;;    C# 側 [STAThread] Main 上で Application を作って Run する。
;;;;  - 開発: dotcl から experiments/dev-run.lisp が (build-main-window) を
;;;;    自前 STA スレッドで Show する (高速反復用)。
;;;;
;;;; どちらでも動くよう、WPF type へのアクセスは ensure-wpf で吸収する
;;;; (host = Desktop ランタイム上なので単純名で解決、script = フルパス fallback)。

(defpackage :paalam
  (:use :cl)
  (:export :build-app :build-main-window :ensure-wpf))
(in-package :paalam)

(defvar *wpf-ready* nil)

(defun %latest-windowsdesktop-dir ()
  (let ((dirs (directory
               "C:/Program Files/dotnet/shared/Microsoft.WindowsDesktop.App/*/")))
    (when dirs
      (flet ((ver (p) (car (last (pathname-directory p)))))
        (let ((best (first dirs)))
          (dolist (p (rest dirs)) (when (string< (ver best) (ver p)) (setf best p)))
          (namestring best))))))

(defun ensure-wpf ()
  "WPF アセンブリを利用可能にする。host では既にロード済み/単純名で解決でき、
script では WindowsDesktop.App からフルパス load する。一度成功したら no-op。"
  (when *wpf-ready* (return-from ensure-wpf t))
  ;; 型が解決できるか静的プロパティで試す (Application を new しない —
  ;; WPF は 1 AppDomain 1 Application なので probe で消費してはいけない)。
  (handler-case
      (progn
        (dotnet:static "System.Windows.Application" "Current") ; 解決可否のみ
        (setf *wpf-ready* t))
    (error ()
      ;; fallback: WindowsDesktop.App からフルパス load。
      (let ((dir (%latest-windowsdesktop-dir)))
        (unless dir (error "Microsoft.WindowsDesktop.App not found"))
        (dolist (dll '("WindowsBase.dll" "PresentationCore.dll"
                       "PresentationFramework.dll" "System.Xaml.dll"))
          (dotnet:load-assembly (concatenate 'string dir dll)))
        (setf *wpf-ready* t))))
  *wpf-ready*)

(defparameter *default-image* "C:/Windows/System32/@WLOGO_96x96.png"
  "起動時に表示するプレースホルダ画像 (ページ送り実装までの仮)。")

(defun build-main-window (&optional (img *default-image*))
  "画像 1 枚を Uniform フィットで表示する WPF Window を作って返す。
呼び出しスレッド (STA であること) 上で構築する。"
  (ensure-wpf)
  (let* ((uri  (dotnet:new "System.Uri" img))
         (bmp  (dotnet:new "System.Windows.Media.Imaging.BitmapImage" uri))
         (win  (dotnet:new "System.Windows.Window"))
         (ictl (dotnet:new "System.Windows.Controls.Image")))
    (dotnet:invoke ictl "set_Source" bmp)
    (dotnet:invoke ictl "set_Stretch"
                   (dotnet:static "System.Windows.Media.Stretch" "Uniform"))
    (dotnet:invoke win "set_Content" ictl)
    (dotnet:invoke win "set_Title"
                   (format nil "paalam — ~a" (file-namestring img)))
    (dotnet:invoke win "set_Width"  800d0)
    (dotnet:invoke win "set_Height" 600d0)
    win))

(defun %maybe-autoclose (app)
  "環境変数 DOTCL_AUTOCLOSE_MS があれば、その ms 後に app.Shutdown する
DispatcherTimer を仕込む (動作確認用)。"
  (let ((s (dotnet:static "System.Environment" "GetEnvironmentVariable"
                          "DOTCL_AUTOCLOSE_MS")))
    (when (and s (> (length s) 0))
      (let ((ms (parse-integer s :junk-allowed t))
            (timer (dotnet:new "System.Windows.Threading.DispatcherTimer")))
        (when ms
          (dotnet:invoke timer "set_Interval"
                         (dotnet:static "System.TimeSpan" "FromMilliseconds"
                                        (coerce ms 'double-float)))
          (dotnet:add-event timer "Tick"
                            (lambda (sndr e) (declare (ignore sndr e))
                              (dotnet:invoke timer "Stop")
                              (dotnet:invoke app "Shutdown")))
          (dotnet:invoke timer "Start"))))))

(defun build-app ()
  "WPF host 入口。Application を作り MainWindow を設定して Application を返す。
C# 側 [STAThread] Main がこの戻り値を受け取り app.Run() する。"
  (ensure-wpf)
  ;; Application は 1 AppDomain 1 個。既存 (Application.Current) があれば再利用。
  (let ((app (or (dotnet:static "System.Windows.Application" "Current")
                 (dotnet:new "System.Windows.Application")))
        (win (build-main-window)))
    (dotnet:invoke app "set_MainWindow" win)
    (dotnet:invoke win "Show")
    (%maybe-autoclose app)
    app))
