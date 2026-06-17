;;;; wpf-smoke.lisp — dotcl で WPF 経路が通るかの段階的検証
;;;;
;;;; 実行: dotcl experiments/wpf-smoke.lisp [image-path]
;;;;
;;;; 知見 (検証で確定):
;;;;  - WPF は WindowsDesktop.App ランタイム上の DLL を *フルパス* で load する
;;;;    (単純名 "WindowsBase" は版不一致 0x80131040 を起こす)。
;;;;  - WPF は STA スレッド必須。Lisp lambda を System.Threading.Thread の
;;;;    ThreadStart として渡せる (dotcl が delegate marshalling する) ので、
;;;;    ui-invoke 無しで自前 STA スレッドを作れる。
;;;;
;;;; Tier1: WindowsDesktop.App から WPF assembly 群を load
;;;; Tier2: BitmapImage デコード (WIC)
;;;; Tier3: STA スレッド上で Window + Image を構築 (Dispatcher 初期化)

(defun report (tier ok &optional detail)
  (format t "[~a] ~a~@[ — ~a~]~%" (if ok "PASS" "FAIL") tier detail))

(defun latest-windowsdesktop-dir ()
  "WindowsDesktop.App の最新バージョンディレクトリ (末尾 / 付き namestring) を返す。"
  (let ((dirs (directory
               "C:/Program Files/dotnet/shared/Microsoft.WindowsDesktop.App/*/")))
    (unless dirs
      (error "Microsoft.WindowsDesktop.App not found"))
    ;; 各 pathname の最後のディレクトリ成分 (= バージョン文字列) で比較。
    (flet ((ver (p) (car (last (pathname-directory p)))))
      (let ((best (first dirs)))
        (dolist (p (rest dirs))
          (when (string< (ver best) (ver p)) (setf best p)))
        (namestring best)))))

(let* ((args (ignore-errors (dotcl:command-line-arguments)))
       (img  (or (find-if (lambda (a)
                            (let ((a (string-downcase a)))
                              (some (lambda (e) (search e a))
                                    '(".png" ".jpg" ".jpeg" ".bmp" ".gif"))))
                          args)
                 "C:/Windows/System32/@WLOGO_96x96.png"))
       (results '()))   ; STA スレッドから push される (Join で同期)
  (block nil

  (format t "==== dotcl WPF smoke test ====~%image: ~a~%~%" img)

  ;; Tier 1: assemblies (フルパス load)
  (handler-case
      (let ((dir (latest-windowsdesktop-dir)))
        (dolist (dll '("WindowsBase.dll" "PresentationCore.dll"
                       "PresentationFramework.dll" "System.Xaml.dll"))
          (dotnet:load-assembly (concatenate 'string dir dll)))
        (report "Tier1 load-assembly" t (format nil "from ~a" dir)))
    (error (e)
      (report "Tier1 load-assembly" nil (princ-to-string e))
      (format t "~%==== aborted ====~%")
      (return)))

  ;; Tier 2 & 3 は STA スレッド上で
  (let ((th (dotnet:new "System.Threading.Thread"
              (lambda ()
                ;; Tier 2: BitmapImage decode
                (handler-case
                    (let* ((uri (dotnet:new "System.Uri" img))
                           (bmp (dotnet:new "System.Windows.Media.Imaging.BitmapImage" uri))
                           (w   (dotnet:invoke bmp "get_PixelWidth"))
                           (h   (dotnet:invoke bmp "get_PixelHeight")))
                      (push (list "Tier2 BitmapImage" t
                                  (format nil "decoded ~ax~a px" w h))
                            results)
                      ;; Tier 3: Window + Image
                      (handler-case
                          (let ((win  (dotnet:new "System.Windows.Window"))
                                (ictl (dotnet:new "System.Windows.Controls.Image")))
                            (dotnet:invoke ictl "set_Source" bmp)
                            (dotnet:invoke win "set_Content" ictl)
                            (dotnet:invoke win "set_Title" "paalam wpf smoke")
                            (dotnet:invoke win "set_Width"  400d0)
                            (dotnet:invoke win "set_Height" 300d0)
                            (push (list "Tier3 Window+Image" t
                                        (format nil "Window title=~s"
                                                (dotnet:invoke win "get_Title")))
                                  results))
                        (error (e)
                          (push (list "Tier3 Window+Image" nil (princ-to-string e))
                                results))))
                  (error (e)
                    (push (list "Tier2 BitmapImage" nil (princ-to-string e))
                          results)))))))
    (dotnet:invoke th "SetApartmentState"
                   (dotnet:static "System.Threading.ApartmentState" "STA"))
    (dotnet:invoke th "Start")
    (dotnet:invoke th "Join"))

  (dolist (r (reverse results))
    (apply #'report r))

  (format t "~%==== done ====~%")))
