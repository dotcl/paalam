;;;; app.lisp — Avalonia 最小スパイク (CL-USER で通す)
;;;;
;;;; 検証目的: dotcl が Avalonia の *ジェネリックな起動*
;;;; (AppBuilder.Configure<App>() を dotnet:static-generic) を駆動して、
;;;; クロスプラット (net10.0, -windows 不要) で画像 1 枚の窓を出せるか。
;;;;
;;;; define-class は DotCL.Runtime NuGet に同梱されない contrib なので vendor して load。

(load (concatenate 'string
                   (dotnet:static "System.AppContext" "BaseDirectory")
                   "vendor-dotnet-class.lisp"))

(defparameter *image*
  (or (let ((e (dotnet:static "System.Environment" "GetEnvironmentVariable" "PAALAM_IMAGE")))
        (and e (plusp (length e)) e))
      "C:/Windows/System32/@WLOGO_96x96.png"))

(defun build-av-window ()
  "Avalonia の Window に画像 1 枚を Uniform フィットで表示して返す。"
  (let ((win (dotnet:new "Avalonia.Controls.Window"))
        (img (dotnet:new "Avalonia.Controls.Image"))
        (bmp (dotnet:new "Avalonia.Media.Imaging.Bitmap" *image*)))
    (dotnet:invoke img "set_Source" bmp)
    (dotnet:invoke img "set_Stretch" (dotnet:static "Avalonia.Media.Stretch" "Uniform"))
    (dotnet:invoke win "set_Content" img)
    (dotnet:invoke win "set_Title" "paalam avalonia spike")
    (dotnet:invoke win "set_Width"  800d0)
    (dotnet:invoke win "set_Height" 600d0)
    win))

(defun maybe-autoclose (life)
  "DOTCL_AUTOCLOSE_MS があれば その ms 後に lifetime.Shutdown (動作確認用)。"
  (let ((s (dotnet:static "System.Environment" "GetEnvironmentVariable" "DOTCL_AUTOCLOSE_MS")))
    (when (and s (plusp (length s)))
      (let ((ms (parse-integer s :junk-allowed t))
            (timer (dotnet:new "Avalonia.Threading.DispatcherTimer")))
        (when ms
          (dotnet:invoke timer "set_Interval"
                         (dotnet:static "System.TimeSpan" "FromMilliseconds"
                                        (coerce ms 'double-float)))
          (dotnet:add-event timer "Tick"
                            (lambda (s e) (declare (ignore s e))
                              (dotnet:invoke timer "Stop")
                              (dotnet:invoke life "Shutdown" 0)))
          (dotnet:invoke timer "Start"))))))

(defun log-err (where e)
  "真因を掴むため Lisp 側エラーをファイルに残す (dotcl が .NET 内部例外を握り潰すため)。"
  (ignore-errors
    (with-open-file (s (concatenate 'string
                                    (dotnet:static "System.AppContext" "BaseDirectory")
                                    "avspike-lisp-error.log")
                       :direction :output :if-exists :append :if-does-not-exist :create)
      (format s "[~a] ~a~%" where e))))

;;; Avalonia.Application 派生を Lisp 側で emit。
;;; OnFrameworkInitializationCompleted で MainWindow を差す (Avalonia の定型)。
;;; Avalonia 12 の FluentTheme は引数なし ctor を持たず (Uri baseUri = null の
;;; 省略可引数付き)、dotnet:new の 0 引数版 (Activator.CreateInstance(type)) では
;;; 作れない。回避策: 省略可引数を明示的に渡す (nil → null Uri)。N 引数版は
;;; arity 一致で ctor を選ぶので 1 引数 nil が この ctor に当たる。
(dotnet:define-class "Paalam.AvaloniaApp" ("Avalonia.Application")
  (:ctor ()
    (handler-case
        (let ((styles (dotnet:invoke self "get_Styles"))
              (theme (dotnet:new "Avalonia.Themes.Fluent.FluentTheme" nil)))
          (dotnet:invoke styles "Add" theme))
      (error (e) (log-err "ctor" e))))
  (:methods
    ("OnFrameworkInitializationCompleted" () :returns Void :override t
      (handler-case
          (let ((life (dotnet:invoke self "get_ApplicationLifetime"))
                (win  (build-av-window)))
            (dotnet:invoke life "set_MainWindow" win)
            (maybe-autoclose life))
        (error (e) (log-err "OnFrameworkInit" e))))))

(defun run-avalonia ()
  "AppBuilder.Configure<Paalam.AvaloniaApp>().UsePlatformDetect()
   .StartWithClassicDesktopLifetime(args) を dotcl から駆動する。
   StartWith... は窓が閉じる (or autoclose) まで blocking。"
  (let* ((b    (dotnet:static-generic "Avalonia.AppBuilder" "Configure"
                                      (list "Paalam.AvaloniaApp")))
         (b    (dotnet:static "Avalonia.AppBuilderDesktopExtensions" "UsePlatformDetect" b))
         (args (dotnet:static-generic "System.Array" "Empty" (list "System.String"))))
    (dotnet:static "Avalonia.ClassicDesktopStyleApplicationLifetimeExtensions"
                   "StartWithClassicDesktopLifetime"
                   b args
                   (dotnet:static "Avalonia.Controls.ShutdownMode" "OnLastWindowClose"))))
