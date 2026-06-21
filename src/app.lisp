;;;; app.lisp — paalam (#39 main.lisp 分割): build-main-window・App emit・run-paalam
(in-package :paalam)

(defun build-main-window (&optional (target *target-path*))
  "TARGET (既定 *target-path*) の本を開き、見開き対応の Avalonia Window を返す
(呼び出しスレッド上で構築)。2 列 Grid に 2 枚の Image を中央で突き合わせる。
TARGET を引数化したので open-in-new-window から任意パスで別ウィンドウを作れる。"
  (multiple-value-bind (book start) (open-book (%resolve-path target))
    (setf *page-direction* (%book-direction book))   ; 種類で綴じ方向 (#33)
    (let* ((win   (dotnet:new "Avalonia.Controls.Window"))
           (vport (dotnet:new "Avalonia.Controls.Border"))   ; クリップ用ビューポート
           (stage (dotnet:new "Avalonia.Controls.Canvas"))   ; 見開き stage (座標は自前計算; #15)
           (li    (dotnet:new "Avalonia.Controls.Image"))
           (ri    (dotnet:new "Avalonia.Controls.Image"))
           (rot   (dotnet:new "Avalonia.Media.RotateTransform"))   ; 回転 (#28)
           (scale (dotnet:new "Avalonia.Media.ScaleTransform"))
           (trans (dotnet:new "Avalonia.Media.TranslateTransform"))
           (grp   (dotnet:new "Avalonia.Media.TransformGroup"))
           (spreads (compute-spreads (book-page-count book)))
           (path  (%resolve-path target))
           ;; 履歴があればそのページから再開 (#7)。
           (start2 (let ((r (history-page path)))
                     (if r (max 0 (min r (1- (book-page-count book)))) start)))
           (vw   (make-viewer :book book :spreads spreads
                              :sindex (spread-index-of spreads start2)
                              :source path
                              :window win :left-img li :right-img ri
                              :viewport vport :scale-xf scale :trans-xf trans :rot-xf rot)))
      ;; Canvas 上にネイティブ寸法で配置 (座標は %render-spread が計算)。Stretch=None。
      (dolist (im (list li ri))
        (dotnet:invoke im "set_Stretch" (dotnet:static "Avalonia.Media.Stretch" "None")))
      (let ((children (dotnet:invoke stage "get_Children")))
        (dotnet:invoke children "Add" li)
        (dotnet:invoke children "Add" ri))
      ;; stage に RenderTransform (実効スケール = フィット基準×zoom + パン)。原点 = 左上、
      ;; これで screen = scale*canvas座標 + translate と単純合成できる。
      (let ((gc (dotnet:invoke grp "get_Children")))
        (dotnet:invoke gc "Add" rot)       ; 回転 → スケール → 平行移動 の順 (#28)
        (dotnet:invoke gc "Add" scale)
        (dotnet:invoke gc "Add" trans))
      (dotnet:invoke stage "set_RenderTransform" grp)
      (dotnet:invoke stage "set_RenderTransformOrigin"
                     (dotnet:static "Avalonia.RelativePoint" "TopLeft"))
      ;; viewport (clip) > stage
      ;; Focusable にしておき、起動時/メニュー操作後にここへフォーカスを置くことで
      ;; 矢印等のキーが必ず Window の KeyDown まで届く (メニューに吸われない)。
      (dotnet:invoke vport "set_Focusable" t)
      (dotnet:invoke vport "set_ClipToBounds" t)
      (dotnet:invoke vport "set_Child" stage)
      ;; リサイズで基準スケール/アンカーを再計算 (#15)。
      (dotnet:add-event vport "SizeChanged"
                        (lambda (s e) (declare (ignore s e)) (%apply-fit vw)))
      ;; root: [viewport* | 素材ペイン(Auto, 既定非表示)] (#18 Phase 2)
      (let* ((root  (dotnet:new "Avalonia.Controls.Grid"))
             (btree (dotnet:new "Avalonia.Controls.TreeView"))      ; フォルダツリー (#6 B)
             (tpane (dotnet:new "Avalonia.Controls.ScrollViewer"))  ; ページサムネ本棚 (#6 A)
             (tstack (dotnet:new "Avalonia.Controls.StackPanel"))
             (pane  (dotnet:new "Avalonia.Controls.DockPanel"))
             (ebtn  (dotnet:new "Avalonia.Controls.Button"))
             (pbtn  (dotnet:new "Avalonia.Controls.Button"))   ; プレビュー (クリップを本として開く; #35 ⑤)
             (scr   (dotnet:new "Avalonia.Controls.ScrollViewer"))
             (stack (dotnet:new "Avalonia.Controls.StackPanel"))
             (overlay (dotnet:new "Avalonia.Controls.Border")))   ; DnD ドロップ可否ハイライト枠 (#20)
        ;; 4 列: [フォルダ(Auto) | サムネ(Auto) | viewport(star) | 素材(Auto)]。Auto は非表示で潰れる。
        (let ((cols (dotnet:invoke root "get_ColumnDefinitions")))
          (dolist (w (list "Auto" "Auto" "*" "Auto"))
            (let ((c (dotnet:new "Avalonia.Controls.ColumnDefinition")))
              (unless (string= w "*")
                (dotnet:invoke c "set_Width" (dotnet:static "Avalonia.Controls.GridLength" "Auto")))
              (dotnet:invoke cols "Add" c))))
        ;; フォルダツリーペイン (左端, 既定非表示)。選択=展開ナビ、ダブルクリック=開く。
        (dotnet:invoke btree "set_Width" 220d0)
        (dotnet:invoke btree "set_IsVisible" nil)
        (dotnet:invoke btree "set_Background" (dotnet:static "Avalonia.Media.Brushes" "DimGray"))
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" btree 0)
        (setf (viewer-browse-pane vw) btree (viewer-browse-tree vw) btree)
        (dotnet:add-event btree "SelectionChanged"
          (lambda (s e) (declare (ignore s e))
            (let ((item (dotnet:invoke btree "get_SelectedItem")))
              (when item
                (let ((path (dotnet:invoke item "get_Tag")))
                  (when (and path (dotnet:static "System.IO.Directory" "Exists" path))
                    (%browse-populate item path)
                    (dotnet:invoke item "set_IsExpanded" t)))))))
        (dotnet:add-event btree "DoubleTapped"
          (lambda (s e) (declare (ignore s e))
            (let ((item (dotnet:invoke btree "get_SelectedItem")))
              (when item
                (let ((path (dotnet:invoke item "get_Tag")))
                  (when path (ignore-errors (open-path vw path)) (%focus-viewport vw)))))))
        ;; サムネペイン (既定非表示)
        (dotnet:invoke tpane "set_Width" 180d0)
        (dotnet:invoke tpane "set_IsVisible" nil)
        (dotnet:invoke tpane "set_Background" (dotnet:static "Avalonia.Media.Brushes" "DimGray"))
        (dotnet:invoke tpane "set_Content" tstack)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" tpane 1)
        (setf (viewer-thumb-pane vw) tpane (viewer-thumb-stack vw) tstack)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" vport 2)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" pane  3)
        (dotnet:invoke pane "set_Width" 220d0)
        (dotnet:invoke pane "set_IsVisible" nil)
        (dotnet:invoke pane "set_Background" (dotnet:static "Avalonia.Media.Brushes" "DimGray"))
        ;; DnD ハイライト枠: 全面を覆う太枠 (中身は透明)。当たり判定を切ってドロップを邪魔しない。
        (dotnet:invoke overlay "set_BorderBrush" (dotnet:static "Avalonia.Media.Brushes" "DeepSkyBlue"))
        (dotnet:invoke overlay "set_BorderThickness" (dotnet:new "Avalonia.Thickness" 4d0))
        (dotnet:invoke overlay "set_IsHitTestVisible" nil)
        (dotnet:invoke overlay "set_IsVisible" nil)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" overlay 0)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumnSpan" overlay 4)
        (dotnet:invoke ebtn "set_Content" "掃き出し")
        (dotnet:add-event ebtn "Click" (lambda (s e) (declare (ignore s e)) (%pane-export vw)))
        (dotnet:invoke pbtn "set_Content" "プレビュー")
        (dotnet:add-event pbtn "Click" (lambda (s e) (declare (ignore s e)) (%open-clip-as-book vw)))
        (dotnet:invoke scr "set_Content" stack)
        (setf (viewer-clip-scroll vw) scr)   ; カーソル追従スクロール用 (#35 ④)
        ;; クリップ編集の vi キー (#35 ④ Phase 1)。stack に focus 中だけ効く局所ハンドラ。
        ;; Handled=t で窓の %on-key (P=ペイン/D=方向 等) に漏らさない (キー衝突回避)。
        (dotnet:invoke stack "set_Focusable" t)
        (dotnet:add-event stack "KeyDown"
          (lambda (s e) (declare (ignore s))
            (let* ((kn   (dotnet:invoke (dotnet:invoke e "get_Key") "ToString"))
                   (d    (%digit-key kn))
                   (cnt  (viewer-clip-count vw))
                   (pend (viewer-clip-pending vw)))
              (flet ((h () (dotnet:invoke e "set_Handled" t))
                     (clear () (setf (viewer-clip-count vw) nil (viewer-clip-pending vw) nil)))
                (cond
                  ;; count 蓄積 (1-9 開始 / 継続中の 0)
                  ((and d (or (> d 0) cnt))
                   (h) (setf (viewer-clip-pending vw) nil
                             (viewer-clip-count vw) (+ (* 10 (or cnt 0)) d)))
                  ((member kn '("J" "Down") :test #'string=) (h) (%clip-cursor-move vw (or cnt 1)) (clear))
                  ((member kn '("K" "Up")   :test #'string=) (h) (%clip-cursor-move vw (- (or cnt 1))) (clear))
                  ;; dd = 切り取り (二連)。count は 1 連目で保持。
                  ((string= kn "D") (h) (if (eq pend :d) (progn (%clip-cut vw cnt) (clear))
                                           (setf (viewer-clip-pending vw) :d)))
                  ;; yy = コピー (二連)
                  ((string= kn "Y") (h) (if (eq pend :y) (progn (%clip-yank vw cnt) (clear))
                                           (setf (viewer-clip-pending vw) :y)))
                  ;; x = 切り取り (単発/count/選択)
                  ((member kn '("X" "Delete") :test #'string=) (h) (%clip-cut vw cnt) (clear))
                  ;; p=後ろ / P(Shift)=手前 に貼り付け (別クリップへ移植可)
                  ((string= kn "P") (h) (%clip-paste vw (%shift? e)) (clear))
                  ;; Space = 選択トグル
                  ((string= kn "Space") (h) (%clip-toggle-select vw) (clear))
                  ((string= kn "Escape")
                   (h) (if (or (viewer-clip-selected vw) pend cnt)
                           (progn (setf (viewer-clip-selected vw) nil) (clear) (%clip-cursor-highlight vw))
                           (%focus-viewport vw)))
                  ;; 未知キーは状態だけリセットして素通し (F11 等のグローバルを活かす)
                  (t (clear)))))))
        (let ((pc (dotnet:invoke pane "get_Children"))
              (cbar (%build-clip-bar vw))           ; クリップ選択バー (#35 ③)
              (actions (dotnet:new "Avalonia.Controls.StackPanel")))   ; [プレビュー][掃き出し] (#35 ⑤)
          (dotnet:invoke actions "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
          (let ((ac (dotnet:invoke actions "get_Children")))
            (dotnet:invoke ac "Add" pbtn) (dotnet:invoke ac "Add" ebtn))
          (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" cbar
                         (dotnet:static "Avalonia.Controls.Dock" "Top"))
          (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" actions
                         (dotnet:static "Avalonia.Controls.Dock" "Top"))
          (dotnet:invoke pc "Add" cbar)
          (dotnet:invoke pc "Add" actions)
          (dotnet:invoke pc "Add" scr))
        (let ((rc (dotnet:invoke root "get_Children")))
          (dotnet:invoke rc "Add" btree)
          (dotnet:invoke rc "Add" tpane)
          (dotnet:invoke rc "Add" vport)
          (dotnet:invoke rc "Add" pane)
          (dotnet:invoke rc "Add" overlay)   ; 最後に追加 = 最前面
          ;; : コマンドバー (全列スパン・下部・最前面; #23)
          (let ((cb (%make-cmdbar vw)))
            (dotnet:static "Avalonia.Controls.Grid" "SetColumn" cb 0)
            (dotnet:static "Avalonia.Controls.Grid" "SetColumnSpan" cb 4)
            (dotnet:invoke rc "Add" cb))
          ;; ステータス表示 (全列スパン・上部中央・最前面; 全画面でもメッセージが見える; #23)
          (let ((st (%make-status vw)))
            (dotnet:static "Avalonia.Controls.Grid" "SetColumn" st 0)
            (dotnet:static "Avalonia.Controls.Grid" "SetColumnSpan" st 4)
            (dotnet:invoke rc "Add" st)))
        (setf (viewer-clip-pane vw) pane (viewer-clip-stack vw) stack
              (viewer-dnd-overlay vw) overlay)
        ;; ペインへの DnD で素材追加 (#18 Phase 2b)。DnD 中はペイン背景を強調 (#20)。
        (%dnd-enable-clip pane
                          (lambda (p) (%clip-drop vw p))
                          (lambda (on)
                            (dotnet:invoke pane "set_Background"
                                           (dotnet:static "Avalonia.Media.Brushes"
                                                          (if on "SteelBlue" "DimGray")))))
        ;; メニューバー (#21): Window.Content を DockPanel [Menu(Top) | root] に包む。
        (dotnet:invoke vport "set_ContextMenu" (%build-context-menu vw))   ; 右クリックメニュー (#26; ジェスチャ抑止込み #29)
        (let ((dock (dotnet:new "Avalonia.Controls.DockPanel"))
              (menu (%build-menu vw win)))
          (setf (viewer-menu vw) menu)
          (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" menu
                         (dotnet:static "Avalonia.Controls.Dock" "Top"))
          (let ((dc (dotnet:invoke dock "get_Children")))
            (dotnet:invoke dc "Add" menu)
            (dotnet:invoke dc "Add" root))
          (dotnet:invoke win "set_Content" dock)))
      (%apply-background vw)          ; 背景色 (= 余白/レターボックス, #2)
      (%apply-interpolation vw)       ; 拡大補間品質 (#2)
      ;; ウィンドウ/タスクバーのアイコン (出力直下の app.ico)。
      (ignore-errors
        (let ((ico (concatenate 'string (dotnet:static "System.AppContext" "BaseDirectory") "app.ico")))
          (when (dotnet:static "System.IO.File" "Exists" ico)
            (dotnet:invoke win "set_Icon" (dotnet:new "Avalonia.Controls.WindowIcon" ico)))))
      (dotnet:invoke win "set_Width"  1200d0)
      (dotnet:invoke win "set_Height" 800d0)
      (dotnet:add-event win "KeyDown"
                        (lambda (s e) (declare (ignore s)) (%on-key vw e)))
      ;; 起動直後からキー操作が効くよう、表示時にビューポートへフォーカス。
      (dotnet:add-event win "Opened"
                        (lambda (s e) (declare (ignore s e)) (%focus-viewport vw)))
      (dotnet:add-event win "PointerWheelChanged"
                        (lambda (s e) (declare (ignore s)) (%on-wheel vw e)))
      (dotnet:add-event win "PointerPressed"
                        (lambda (s e) (declare (ignore s)) (%on-press vw e)))
      (dotnet:add-event win "PointerMoved"
                        (lambda (s e) (declare (ignore s)) (%on-move vw e)))
      (dotnet:add-event win "PointerReleased"
                        (lambda (s e) (declare (ignore s)) (%on-release vw e)))
      (dotnet:add-event win "Closed"
                        (lambda (s e) (declare (ignore s e))
                          (ignore-errors (save-history))   ; 最終ページを永続化 (#7)
                          (ignore-errors (loader-stop (viewer-loader vw)))
                          ;; open-path で差し替え後も「現在の本」を閉じる (初期 book ではなく)。
                          (ignore-errors (book-close (viewer-book vw)))
                          ;; マルチウィンドウ: この viewer をリストから外す。
                          (setf *viewers* (remove vw *viewers*))))
      ;; ドラッグ&ドロップで別の本を開く (#19)。配線は C# シムに封じ込め。
      ;; DnD 中はウィンドウ全面にハイライト枠を出す (#20)。
      (%dnd-enable win
                   (lambda (p) (open-path vw p))
                   (lambda (on) (dotnet:invoke (viewer-dnd-overlay vw) "set_IsVisible" (and on t))))
      ;; 先読みローダーを起動 (vw を closure で参照するので vw 作成後)。
      (setf (viewer-loader vw)
            (open-loader (let ((p path)) (lambda () (open-book p)))
                         (lambda (index)
                           (%ui-post (lambda () (%on-page-loaded vw index))))))
      (show-spread vw)
      (push vw *viewers*)   ; マルチウィンドウ: 開いている viewer を登録 (Closed で remove)
      win)))

(defun open-in-new-window (path)
  "PATH を新しいウィンドウで開く (UI スレッド上で呼ぶこと)。macOS の activation で
2 個目以降のファイルを別ウィンドウに開くのに使う。初回ウィンドウは lifetime が
set_MainWindow で表示するが、2 枚目以降は明示 Show が要る。"
  (let ((win (build-main-window path)))
    (dotnet:invoke win "Show")
    win))

(defun %activate-open (path)
  "macOS の Finder「開く」/Dock ドロップで来た PATH を開く (UI スレッド上)。
- まだ何も開いていない空ウィンドウ (viewer-source = nil) があれば、それを差し替える
  (未起動→1ファイルで空窓+ファイル窓の2枚にならないように)。
- 空ウィンドウが無ければ新しいウィンドウで開く (複数ファイルは1件ずつ来る)。
- 初回 build 前 (*viewers* 空) なら *target-path* に積む (起動シーケンスが開く)。"
  (cond
    ((null *viewers*) (setf *target-path* path))
    (t (let ((empty (find-if (lambda (vw) (null (viewer-source vw))) *viewers*)))
         (if empty
             (open-path empty path)
             (open-in-new-window path))))))

(defun %maybe-autoclose (life)
  "デバッグ用: PAALAM_DEBUG 有効時に限り DOTCL_AUTOCLOSE_MS (正の ms) 後に
lifetime.Shutdown する (起動確認の自動終了)。本番起動 (Finder/launchd 等) で
環境にゴミの DOTCL_AUTOCLOSE_MS が残っていても誤って即終了しないよう、
*debug* と「正の整数 ms」の両方を満たすときだけ作動させる。"
  (when *debug*
    (let ((s (dotnet:static "System.Environment" "GetEnvironmentVariable" "DOTCL_AUTOCLOSE_MS")))
      (when (and s (plusp (length s)))
        (let ((ms (parse-integer s :junk-allowed t))
              (timer (dotnet:new "Avalonia.Threading.DispatcherTimer")))
          (when (and ms (plusp ms))   ; 0 や負値・非数では発火しない
            (dotnet:invoke timer "set_Interval"
                           (dotnet:static "System.TimeSpan" "FromMilliseconds"
                                          (coerce ms 'double-float)))
            (dotnet:add-event timer "Tick"
                              (lambda (sndr e) (declare (ignore sndr e))
                                (dotnet:invoke timer "Stop")
                                (dotnet:invoke life "Shutdown" 0)))
            (dotnet:invoke timer "Start")))))))

;;; Avalonia.Application 派生を Lisp 側で emit。
(dotnet:define-class "Paalam.App" ("Avalonia.Application")
  (:ctor ()
    ;; Fluent テーマ (将来のメニュー/ツールバー等テンプレートコントロール用)。
    ;; 0.1.11 で dotnet:new が全 optional 引数 ctor を補えるので引数なしで OK (#292 解決)。
    (handler-case
        (let ((styles (dotnet:invoke self "get_Styles"))
              (theme (dotnet:new "Avalonia.Themes.Fluent.FluentTheme")))
          (dotnet:invoke styles "Add" theme)
          (%apply-theme))              ; OS 追従 or 固定テーマを反映 (#27)
      (error (e) (log-err "ctor" e))))
  (:methods
    ("OnFrameworkInitializationCompleted" () :returns Void :override t
      (handler-case
          (let ((life (dotnet:invoke self "get_ApplicationLifetime"))
                (win  (build-main-window)))
            (dotnet:invoke life "set_MainWindow" win)
            ;; macOS の Finder「開く」/ Dock ドロップ / 関連付け起動で来るファイルを受ける。
            ;; viewer があれば差し替え、無ければ次の build で開く target に積む。
            (dotnet:static "Paalam.FileOpenHelper" "Enable" self
                           (dotnet:make-delegate "System.Action`1[System.String]"
                             (lambda (p)
                               (%ui-post (lambda () (%activate-open p))))))
            (%maybe-autoclose life))
        (error (e) (log-err "init" e))))))

(defun run-paalam (&optional target)
  "host 入口。AppBuilder.Configure<Paalam.App>().UsePlatformDetect()
   .StartWithClassicDesktopLifetime(args) を駆動する (窓が閉じるまで blocking)。"
  (setf *debug* (let ((e (dotnet:static "System.Environment" "GetEnvironmentVariable" "PAALAM_DEBUG")))
                  (and e (plusp (length e)) t)))
  (load-settings)
  (load-keybinds)
  (load-menu)
  (load-history)
  (load-bookmarks)
  (load-clip)
  (load-clip-cache)                    ; #35 ①: クリップのローカルキャッシュ目録
  (%clip-cache-warm)                   ; 既存クリップで未キャッシュ分を背景で埋める
  (load-gestures)
  (setf *target-path* target)
  (let* ((b    (dotnet:static-generic "Avalonia.AppBuilder" "Configure" (list "Paalam.App")))
         (b    (dotnet:static "Avalonia.AppBuilderDesktopExtensions" "UsePlatformDetect" b))
         ;; macOS: CVDisplayLink RenderTimer の起動失敗 (-6661, Avalonia #14927) を避けるため
         ;; Software レンダリングに固定する (Finder 起動 / 蓋閉じ等で再現する -6661 対策)。
         (b    (if (dotnet:static "System.OperatingSystem" "IsMacOS")
                   (dotnet:static "Paalam.MacRenderFix" "UseSoftwareRendering" b)
                   b))
         (args (dotnet:static-generic "System.Array" "Empty" (list "System.String"))))
    (dotnet:static "Avalonia.ClassicDesktopStyleApplicationLifetimeExtensions"
                   "StartWithClassicDesktopLifetime"
                   b args
                   (dotnet:static "Avalonia.Controls.ShutdownMode" "OnLastWindowClose"))))
