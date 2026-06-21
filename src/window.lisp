;;;; window.lisp — paalam (#39 main.lisp 分割): ウィンドウ操作・設定ウィンドウ補助・関連付け
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; ウィンドウ / アプリ (Avalonia)
;;; ------------------------------------------------------------------

(defvar *target-path* nil
  "RUN-PAALAM が受け取った対象パス (App は Avalonia が生成するので global で渡す)。")

(defvar *viewers* nil
  "開いている全ウィンドウの viewer リスト。build-main-window で push、
ウィンドウ Closed で remove する。macOS の Finder「開く」(IActivatableLifetime)
で起動済みのとき、新しいファイルを別ウィンドウで開くか (空なら *target-path* に積む)
の判定に使う。Windows/Linux は OS がファイル毎に別プロセスを起動するので、
この activation 経路は通らず実質 macOS 専用。")

(defun %resolve-path (path)
  "対象パスを決める。指定が無ければ nil を返す (= 本を開かず空の窓を出す)。
以前は Windows 固定パス (@WLOGO_96x96.png) をフォールバックにしていたが、
他 OS では存在せず open-book が例外 → 初期化中断で窓が出なかったため廃止。"
  (or (and (stringp path) (plusp (length path)) path)
      (let ((env (dotnet:static "System.Environment" "GetEnvironmentVariable" "PAALAM_PATH")))
        (and env (plusp (length env)) env))))

;;; 表示トグル (キー / メニュー 共用)
(defun %toggle-spread (vw)
  (setf *spread* (not *spread*)) (%recompute vw (%cur-page vw)) (save-settings))
(defun %toggle-direction (vw)
  (setf *page-direction* (if (eq *page-direction* :rtl) :ltr :rtl))
  (show-spread vw) (save-settings))
(defun %toggle-recursive (vw)
  (setf *recursive* (not *recursive*)) (save-settings) (%reopen vw))
(defun %cycle-fit-mode (vw)
  "フィットモードを 全体→幅→高→全体 と巡回し、フィットし直す (#15)。"
  (setf *fit-mode* (ecase *fit-mode* (:whole :width) (:width :height) (:height :whole)))
  (save-settings)
  (reset-view vw)
  (%update-title vw))

(defun %open-file-dialog (vw win)
  (dotnet:static "Paalam.DialogHelper" "PickOpenBook" win
                 (dotnet:make-delegate "System.Action`1[System.String]"
                                       (lambda (p) (open-path vw p)))))
(defun %open-folder-dialog (vw win)
  (dotnet:static "Paalam.DialogHelper" "PickOpenFolder" win
                 (dotnet:make-delegate "System.Action`1[System.String]"
                                       (lambda (p) (open-path vw p)))))

;;; 設定ウィンドウ (#9): メニュー「設定…」から別ウィンドウで開く。OK で確定保存、
;;; キャンセルで破棄 (グローバル特殊変数は OK を押すまで一切触らない)。永続化基盤
;;; (*settings*/save-settings, #8) にそのまま乗せ、反映は変更内容に応じて再描画。
(defun %parse-int (s default min)
  "S を整数に。パース失敗 or MIN 未満なら DEFAULT を返す。"
  (let ((v (ignore-errors (parse-integer (string-trim '(#\Space #\Tab) s) :junk-allowed t))))
    (if (and v (>= v min)) v default)))

(defun %s-header (text)
  (let ((tb (dotnet:new "Avalonia.Controls.TextBlock")))
    (dotnet:invoke tb "set_Text" text)
    (dotnet:invoke tb "set_FontWeight" (dotnet:static "Avalonia.Media.FontWeight" "Bold"))
    (dotnet:invoke tb "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 10d0 0d0 4d0))
    tb))

(defun %s-row (label control)
  "[label(固定幅) ........ control] の横並び行。"
  (let ((row (dotnet:new "Avalonia.Controls.DockPanel"))
        (tb  (dotnet:new "Avalonia.Controls.TextBlock")))
    (dotnet:invoke tb "set_Text" label)
    (dotnet:invoke tb "set_Width" 170d0)
    (dotnet:invoke tb "set_VerticalAlignment"
                   (dotnet:static "Avalonia.Layout.VerticalAlignment" "Center"))
    (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" tb
                   (dotnet:static "Avalonia.Controls.Dock" "Left"))
    (dotnet:invoke row "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 3d0))
    (let ((c (dotnet:invoke row "get_Children")))
      (dotnet:invoke c "Add" tb)
      (dotnet:invoke c "Add" control))
    row))

(defun %set-check (cb on)
  "CheckBox.IsChecked (bool?) を true/false に。#305 (setter, D1225 / 0.1.13-pkgtest3) で
bool? プロパティ setter も nil→false になったので boxed-false シムを撤去 (true=t / false=nil)。
indeterminate が要るなら (dotnet:null)。"
  (dotnet:invoke cb "set_IsChecked" (if on t nil)))

(defun %s-checkbox (checked)
  (let ((cb (dotnet:new "Avalonia.Controls.CheckBox")))
    (dotnet:invoke cb "set_IsThreeState" nil)
    (%set-check cb checked) cb))

(defun %s-textbox (n)
  (let ((tb (dotnet:new "Avalonia.Controls.TextBox")))
    (dotnet:invoke tb "set_Text" (format nil "~d" n))
    (dotnet:invoke tb "set_Width" 80d0)
    (dotnet:invoke tb "set_HorizontalAlignment"
                   (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Left"))
    tb))

(defun %s-button (label click)
  (let ((b (dotnet:new "Avalonia.Controls.Button")))
    (dotnet:invoke b "set_Content" label)
    (dotnet:invoke b "set_MinWidth" 80d0)
    (dotnet:invoke b "set_Margin" (dotnet:new "Avalonia.Thickness" 6d0 0d0 0d0 0d0))
    (dotnet:add-event b "Click" (lambda (s e) (declare (ignore s e)) (funcall click)))
    b))

;;; 設定コンボの選択肢 (表示ラベル . 値)。
(defparameter *dir-choices*    '(("右開き (RTL)" . :rtl) ("左開き (LTR)" . :ltr)))
(defparameter *fit-choices*    '(("全体" . :whole) ("幅フィット" . :width) ("高さフィット" . :height)))
(defparameter *bg-choices*     '(("黒" . "Black") ("濃い灰" . "DimGray") ("灰" . "Gray") ("白" . "White")))
(defparameter *interp-choices*
  '(("高品質 (なめらか)" . :high) ("低品質" . :low) ("なし (ドット絵/最近傍)" . :none)))
(defparameter *theme-choices*
  '(("システム追従 (OS のライト/ダーク)" . :system) ("ライト" . :light) ("ダーク" . :dark)))
(defparameter *export-width-choices*
  '(("原寸 (変換なし)" . :original) ("最大 1080px" . 1080) ("最大 1600px" . 1600) ("最大 2048px" . 2048))
  "掃き出し最大幅の選択肢 (#35 ②)。")

(defun %s-choice-combo (choices current &optional (test #'eql))
  "(ラベル . 値) の CHOICES から ComboBox を作り、CURRENT の項目を選択。"
  (let ((c (dotnet:new "Avalonia.Controls.ComboBox")))
    (let ((items (dotnet:invoke c "get_Items")))
      (dolist (ch choices) (dotnet:invoke items "Add" (car ch))))
    (dotnet:invoke c "set_SelectedIndex" (or (position current choices :key #'cdr :test test) 0))
    c))

(defun %s-combo-value (combo choices)
  (cdr (nth (max 0 (dotnet:invoke combo "get_SelectedIndex")) choices)))

(defun %prompt-text (owner title initial on-ok)
  "1 行テキスト入力のモーダル。OK/Enter で (funcall on-ok 入力文字列) を呼んでから閉じる。
空白のみは無視。Esc/キャンセルで閉じる。コールバック方式 (ShowDialog の戻り値は使わない)。"
  (let* ((w    (dotnet:new "Avalonia.Controls.Window"))
         (tb   (dotnet:new "Avalonia.Controls.TextBox"))
         (root (dotnet:new "Avalonia.Controls.StackPanel"))
         (btns (dotnet:new "Avalonia.Controls.StackPanel")))
    (labels ((commit ()
               (let ((s (string-trim '(#\Space #\Tab) (or (dotnet:invoke tb "get_Text") ""))))
                 (dotnet:invoke w "Close")
                 (when (plusp (length s)) (funcall on-ok s)))))
      (dotnet:invoke tb "set_Text" (or initial ""))
      (dotnet:invoke tb "set_Width" 240d0)
      (dotnet:add-event tb "KeyDown"
        (lambda (s e) (declare (ignore s))
          (let ((kn (dotnet:invoke (dotnet:invoke e "get_Key") "ToString")))
            (cond ((or (string= kn "Return") (string= kn "Enter"))
                   (dotnet:invoke e "set_Handled" t) (commit))
                  ((string= kn "Escape")
                   (dotnet:invoke e "set_Handled" t) (dotnet:invoke w "Close"))))))
      (dotnet:invoke btns "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
      (dotnet:invoke btns "set_HorizontalAlignment" (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Right"))
      (dotnet:invoke btns "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 12d0 0d0 0d0))
      (let ((c (dotnet:invoke btns "get_Children")))
        (dotnet:invoke c "Add" (%s-button "キャンセル" (lambda () (dotnet:invoke w "Close"))))
        (dotnet:invoke c "Add" (%s-button "OK" (lambda () (commit)))))
      (dotnet:invoke root "set_Margin" (dotnet:new "Avalonia.Thickness" 12d0))
      (let ((c (dotnet:invoke root "get_Children")))
        (dotnet:invoke c "Add" tb)
        (dotnet:invoke c "Add" btns))
      (dotnet:invoke w "set_Content" root)
      (dotnet:invoke w "set_Title" title)
      (dotnet:invoke w "set_SizeToContent" (dotnet:static "Avalonia.Controls.SizeToContent" "WidthAndHeight"))
      (dotnet:invoke w "set_WindowStartupLocation"
                     (dotnet:static "Avalonia.Controls.WindowStartupLocation" "CenterOwner"))
      ;; 表示直後に入力欄へフォーカス&全選択 (Focus のオーバーロード差異は握り潰す)。
      (dotnet:add-event w "Opened"
        (lambda (s e) (declare (ignore s e))
          (ignore-errors (dotnet:invoke tb "Focus") (dotnet:invoke tb "SelectAll"))))
      (dotnet:invoke w "ShowDialog" owner))))

;;; ファイル関連付け (#11, Windows): per-user (HKCU) なので管理者権限不要。ProgID を登録し
;;; 対応拡張子の OpenWithProgids に追加 (「プログラムから開く」に出る)。既定化は OS 仕様で
;;; 自動不可なので、登録後に「既定のアプリ」設定へ誘導する。レジストリ書き込みは Lisp から
;;; Microsoft.Win32.Registry を叩く (C# シムを増やさない方針)。
(defparameter *progid* "Paalam.Document")
(defparameter *assoc-extensions*
  (append *image-extensions* '("zip" "cbz" "rar" "cbr" "7z" "cb7" "tar" "cbt" "pdf"))
  "関連付け対象の拡張子 (paalam が開けるもの)。")

(defun %reg-set (path name value)
  "HKCU\\PATH のサブキー (無ければ作成) に (NAME -> VALUE) を書く。NAME=\"\" で既定値。"
  (let* ((cu (dotnet:static "Microsoft.Win32.Registry" "CurrentUser"))
         (k  (dotnet:invoke cu "CreateSubKey" path)))
    (unwind-protect (dotnet:invoke k "SetValue" name value)
      (ignore-errors (dotnet:invoke k "Close")))))

(defun %assoc-key (ext) (format nil "Software\\Classes\\.~a\\OpenWithProgids" ext))

(defun %ensure-progid ()
  "ProgID 本体 (開くコマンド + アイコン) を登録 (冪等)。exe は自分自身。"
  (let ((exe (dotnet:static "System.Environment" "ProcessPath")))
    (%reg-set (format nil "Software\\Classes\\~a" *progid*) "" "paalam 画像 / 書庫 / PDF")
    (%reg-set (format nil "Software\\Classes\\~a\\DefaultIcon" *progid*) "" (format nil "~a,0" exe))
    (%reg-set (format nil "Software\\Classes\\~a\\shell\\open\\command" *progid*) ""
              (format nil "\"~a\" \"%1\"" exe))))

(defun %assoc-registered-p (ext)
  "EXT の OpenWithProgids に *progid* が載っているか。"
  (handler-case
      (let* ((cu (dotnet:static "Microsoft.Win32.Registry" "CurrentUser"))
             (k  (dotnet:invoke cu "OpenSubKey" (%assoc-key ext))))
        (if k
            (unwind-protect (and (dotnet:invoke k "GetValue" *progid*) t)
              (ignore-errors (dotnet:invoke k "Close")))
            nil))
    (error () nil)))

(defun %assoc-register (exts)
  "EXTS を *progid* の OpenWithProgids 候補に追加 (per-user)。ProgID 本体も用意。"
  (ignore-errors (dotnet:load-assembly "Microsoft.Win32.Registry"))
  (%ensure-progid)
  (dolist (ext exts) (%reg-set (%assoc-key ext) *progid* "")))

(defun %assoc-unregister (exts)
  "EXTS の OpenWithProgids から *progid* を外す。"
  (ignore-errors (dotnet:load-assembly "Microsoft.Win32.Registry"))
  (dolist (ext exts)
    (ignore-errors
      (let* ((cu (dotnet:static "Microsoft.Win32.Registry" "CurrentUser"))
             (k  (dotnet:invoke cu "OpenSubKey" (%assoc-key ext) t)))   ; writable
        (when k
          (unwind-protect
               (ignore-errors (dotnet:invoke k "DeleteValue" *progid* nil)) ; throwOnMissing=false
            (ignore-errors (dotnet:invoke k "Close"))))))))

(defun %open-default-apps ()
  "Windows の「既定のアプリ」設定を開く (既定化はユーザ操作が要るため誘導)。"
  (ignore-errors
    (let ((psi (dotnet:new "System.Diagnostics.ProcessStartInfo")))
      (dotnet:invoke psi "set_FileName" "ms-settings:defaultapps")
      (dotnet:invoke psi "set_UseShellExecute" t)
      (dotnet:static "System.Diagnostics.Process" "Start" psi))))

(defun %settings-apply (vw cb-spread cb-fps cb-rec tb-cache tb-pf tb-th
                        fit-combo bg-combo interp-combo tb-dpi tb-mem tb-slide
                        dir-img-combo dir-pdf-combo theme-combo exp-w-combo)
  "コントロールの現在値をグローバルへ反映し永続化、変更内容に応じて再描画する。"
  (let ((old-spread *spread*) (old-fps *first-page-single*)
        (old-rec *recursive*) (old-threads *loader-threads*) (old-dpi *pdf-dpi*))
    (setf *spread*            (and (dotnet:invoke cb-spread "get_IsChecked") t)
          *first-page-single* (and (dotnet:invoke cb-fps "get_IsChecked") t)
          *recursive*         (and (dotnet:invoke cb-rec "get_IsChecked") t)
          *image-direction*   (%s-combo-value dir-img-combo *dir-choices*)   ; 種類別の既定 (#33)
          *pdf-direction*     (%s-combo-value dir-pdf-combo *dir-choices*)
          ;; 現在の本の綴じ方向も新しい種類別既定で更新 (D で反転した分は上書き)。
          *page-direction*    (%book-direction (viewer-book vw))
          *fit-mode*          (%s-combo-value fit-combo *fit-choices*)
          *background-color*  (%s-combo-value bg-combo *bg-choices*)
          *interpolation*     (%s-combo-value interp-combo *interp-choices*)
          *theme*             (%s-combo-value theme-combo *theme-choices*)
          *export-width*      (%s-combo-value exp-w-combo *export-width-choices*)   ; #35 ②
          *pdf-dpi*           (%parse-int (dotnet:invoke tb-dpi "get_Text") *pdf-dpi* 36)
          *cache-memory-mb*   (%parse-int (dotnet:invoke tb-mem "get_Text") *cache-memory-mb* 16)
          *slideshow-interval* (%parse-int (dotnet:invoke tb-slide "get_Text") *slideshow-interval* 1)
          *cache-size*        (%parse-int (dotnet:invoke tb-cache "get_Text") *cache-size* 1)
          *prefetch-ahead*    (%parse-int (dotnet:invoke tb-pf "get_Text") *prefetch-ahead* 0)
          *loader-threads*    (%parse-int (dotnet:invoke tb-th "get_Text") *loader-threads* 1))
    (save-settings)
    (%apply-theme)               ; UI テーマ (システム追従/ライト/ダーク; #27)
    (%apply-background vw)       ; 背景色 (#2)
    (%apply-interpolation vw)    ; 補間品質 (#2)
    ;; 反映の重さ順: recursive/スレッド数/PDF DPI は本を開き直す (キャッシュを再生成)、
    ;; 見開き構成変化は再計算、それ以外 (綴じ方向/フィット/背景/補間/キャッシュ/先読み) は再描画。
    (cond ((or (not (eq old-rec *recursive*)) (/= old-threads *loader-threads*)
               (/= old-dpi *pdf-dpi*))
           (%reopen vw))
          ((or (not (eq old-spread *spread*)) (not (eq old-fps *first-page-single*)))
           (%recompute vw (%cur-page vw)))
          (t (show-spread vw)))))

(defparameter *modifier-keys*
  '("LeftShift" "RightShift" "LeftCtrl" "RightCtrl" "LeftAlt" "RightAlt" "LWin" "RWin")
  "単体では割当に使わない修飾キー (capture 中は無視して本命キーを待つ)。")

(defun %capture-key (owner on-key)
  "小窓を出し、次に押されたキー名を ON-KEY に渡して閉じる。Esc=キャンセル、修飾キー単体は無視。"
  (let ((w  (dotnet:new "Avalonia.Controls.Window"))
        (tb (dotnet:new "Avalonia.Controls.TextBlock")))
    (dotnet:invoke tb "set_Text" "割り当てるキーを押してください (Esc でキャンセル)")
    (dotnet:invoke tb "set_Margin" (dotnet:new "Avalonia.Thickness" 24d0))
    (dotnet:invoke w "set_Title" "キー割当")
    (dotnet:invoke w "set_SizeToContent"
                   (dotnet:static "Avalonia.Controls.SizeToContent" "WidthAndHeight"))
    (dotnet:invoke w "set_WindowStartupLocation"
                   (dotnet:static "Avalonia.Controls.WindowStartupLocation" "CenterOwner"))
    (dotnet:invoke w "set_Content" tb)
    (dotnet:add-event w "KeyDown"
      (lambda (s e) (declare (ignore s))
        (let ((kn (dotnet:invoke (dotnet:invoke e "get_Key") "ToString")))
          (unless (member kn *modifier-keys* :test #'string=)
            (dotnet:invoke e "set_Handled" t)
            (dotnet:invoke w "Close")
            (unless (string= kn "Escape") (funcall on-key kn))))))
    (dotnet:invoke w "ShowDialog" owner)))

(defun %goto-page (vw)
  "vi 風ページ移動 (#23): [count]g で count ページ目へ。count 無しは末尾へ
 (メニュー/パレットからの直接実行時のフォールバック; キーでは G が末尾を担う)。
count は %on-key が viewer-pending-count に溜めたもの (1g なら 1)。"
  (let* ((n (book-page-count (viewer-book vw)))
         (cnt (viewer-pending-count vw))
         (page (if cnt (1- (max 1 (min cnt n))) (1- n))))
    (when (plusp n) (%jump-to-page vw page))))

