;;;; window-show.lisp — dotcl + WPF で実ウィンドウを表示する最小ビューア
;;;;
;;;; 実行 (対話・閉じるまで表示):
;;;;   dotcl experiments/window-show.lisp [image-path]
;;;; 実行 (自動クローズ・動作確認用):
;;;;   DOTCL_AUTOCLOSE_MS=1500 dotcl experiments/window-show.lisp [image-path]
;;;;
;;;; wpf-smoke.lisp で確定した知見を使う:
;;;;  - WPF DLL は WindowsDesktop.App からフルパス load
;;;;  - 自前 STA スレッド上で Window 構築 → Show → Dispatcher.Run でポンプ

(defun latest-windowsdesktop-dir ()
  (let ((dirs (directory
               "C:/Program Files/dotnet/shared/Microsoft.WindowsDesktop.App/*/")))
    (unless dirs (error "Microsoft.WindowsDesktop.App not found"))
    (flet ((ver (p) (car (last (pathname-directory p)))))
      (let ((best (first dirs)))
        (dolist (p (rest dirs))
          (when (string< (ver best) (ver p)) (setf best p)))
        (namestring best)))))

(defun load-wpf ()
  (let ((dir (latest-windowsdesktop-dir)))
    (dolist (dll '("WindowsBase.dll" "PresentationCore.dll"
                   "PresentationFramework.dll" "System.Xaml.dll"))
      (dotnet:load-assembly (concatenate 'string dir dll)))
    dir))

(defun run-viewer (img autoclose-ms)
  "STA スレッド上で画像 1 枚のウィンドウを表示し Dispatcher を回す。
AUTOCLOSE-MS が非 nil なら そのミリ秒後に自動で閉じる。"
  (let ((th (dotnet:new "System.Threading.Thread"
              (lambda ()
                (let* ((uri  (dotnet:new "System.Uri" img))
                       (bmp  (dotnet:new "System.Windows.Media.Imaging.BitmapImage" uri))
                       (win  (dotnet:new "System.Windows.Window"))
                       (ictl (dotnet:new "System.Windows.Controls.Image"))
                       (disp (dotnet:static "System.Windows.Threading.Dispatcher"
                                            "CurrentDispatcher")))
                  ;; 画像を画面にフィット (Uniform) させる
                  (dotnet:invoke ictl "set_Source" bmp)
                  (dotnet:invoke ictl "set_Stretch"
                                 (dotnet:static "System.Windows.Media.Stretch" "Uniform"))
                  (dotnet:invoke win "set_Content" ictl)
                  (dotnet:invoke win "set_Title"
                                 (format nil "paalam — ~a" (file-namestring img)))
                  (dotnet:invoke win "set_Width"  800d0)
                  (dotnet:invoke win "set_Height" 600d0)
                  ;; ウィンドウを閉じたら Dispatcher を止める
                  (dotnet:add-event win "Closed"
                                    (lambda (s e) (declare (ignore s e))
                                      (dotnet:invoke disp "InvokeShutdown")))
                  (dotnet:invoke win "Show")
                  ;; 自動クローズ用タイマ (動作確認時のみ)
                  (when autoclose-ms
                    (let ((timer (dotnet:new "System.Windows.Threading.DispatcherTimer")))
                      (dotnet:invoke timer "set_Interval"
                                     (dotnet:static "System.TimeSpan" "FromMilliseconds"
                                                    (coerce autoclose-ms 'double-float)))
                      (dotnet:add-event timer "Tick"
                                        (lambda (s e) (declare (ignore s e))
                                          (dotnet:invoke timer "Stop")
                                          (dotnet:invoke win "Close")))
                      (dotnet:invoke timer "Start")))
                  ;; メッセージポンプ。Close → InvokeShutdown で抜ける。
                  (dotnet:static "System.Windows.Threading.Dispatcher" "Run"))))))
    (dotnet:invoke th "SetApartmentState"
                   (dotnet:static "System.Threading.ApartmentState" "STA"))
    (dotnet:invoke th "Start")
    (dotnet:invoke th "Join")))

(let* ((args (ignore-errors (dotcl:command-line-arguments)))
       (img  (or (find-if (lambda (a)
                            (let ((a (string-downcase a)))
                              (some (lambda (e) (search e a))
                                    '(".png" ".jpg" ".jpeg" ".bmp" ".gif"))))
                          args)
                 "C:/Windows/System32/@WLOGO_96x96.png"))
       (ac   (let ((s (dotnet:static "System.Environment" "GetEnvironmentVariable"
                                     "DOTCL_AUTOCLOSE_MS")))
               (and s (> (length s) 0) (parse-integer s :junk-allowed t)))))
  (format t "showing: ~a~@[  (autoclose ~ams)~]~%" img ac)
  (load-wpf)
  (run-viewer img ac)
  (format t "window closed.~%"))
