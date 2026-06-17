;;;; dev-run.lisp — src/main.lisp を dotcl から高速反復するためのラッパ
;;;;
;;;; 実行:  dotcl experiments/dev-run.lisp
;;;;       DOTCL_AUTOCLOSE_MS=1500 dotcl experiments/dev-run.lisp   ; 自動クローズ
;;;;
;;;; host (paalam.exe) は [STAThread] Main + Application.Run でウィンドウを回すが、
;;;; dotcl スクリプト実行時の Main は MTA なので Application.Run が使えない。
;;;; ここでは自前 STA スレッド + window.Show + Dispatcher.Run で同じ
;;;; build-main-window を表示する (REPL/スクリプトでの素早い見た目確認用)。

(load (merge-pathnames "../src/main.lisp" *load-pathname*))

(in-package :paalam)

(let ((th (dotnet:new "System.Threading.Thread"
            (lambda ()
              (let ((win  (build-main-window))
                    (disp (dotnet:static "System.Windows.Threading.Dispatcher"
                                         "CurrentDispatcher")))
                (dotnet:add-event win "Closed"
                                  (lambda (s e) (declare (ignore s e))
                                    (dotnet:invoke disp "InvokeShutdown")))
                (dotnet:invoke win "Show")
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
                                          (lambda (s e) (declare (ignore s e))
                                            (dotnet:invoke timer "Stop")
                                            (dotnet:invoke win "Close")))
                        (dotnet:invoke timer "Start")))))
                (dotnet:static "System.Windows.Threading.Dispatcher" "Run"))))))
  (dotnet:invoke th "SetApartmentState"
                 (dotnet:static "System.Threading.ApartmentState" "STA"))
  (dotnet:invoke th "Start")
  (dotnet:invoke th "Join"))
