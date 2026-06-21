;;;; config-io.lisp — paalam (#39 main.lisp 分割): 設定 import/export (#24) + 設定ウィンドウ
(in-package :paalam)

;;; 設定の import / export (#24): 設定一式を 1 ファイル (s式 plist) に書き出し/読み込み。
;;; 対象: settings / keybinds(keymap) / gestures / menu / bookmarks。未知キーは無視、
;;; 欠けセクションは現状維持。clip/history はパス依存で揮発的なので含めない。
(defun %config-bundle ()
  "現在の設定一式を 1 つの plist にまとめる (export 用)。先頭 :paalam-config がバージョン。"
  (list :paalam-config 1
        :settings (loop for e in *settings*
                        append (list (car e) (symbol-value (cdr e))))
        :keybinds *keybinds*
        :gestures *gestures*
        :menu     *command-menu*
        :bookmarks *bookmarks*))

(defun %apply-config-bundle (vw bundle)
  "import した BUNDLE を反映・永続化する。未知キー無視・欠けセクションは現状維持。"
  (when (and (consp bundle) (getf bundle :paalam-config))
    (let ((s (getf bundle :settings)))
      (when (consp s)
        (loop for (k v) on s by #'cddr
              for e = (assoc k *settings*)
              when e do (setf (symbol-value (cdr e)) v))
        (save-settings)))
    (let ((kb (getf bundle :keybinds)))
      (when kb (setf *keybinds* kb) (rebuild-keymap) (save-keybinds)))
    (let ((g (getf bundle :gestures)))
      (when g (setf *gestures* g) (save-gestures)))
    (let ((m (getf bundle :menu)))
      (when m (setf *command-menu* m) (save-menu) (%rebuild-menus vw)))
    (let ((b (getf bundle :bookmarks)))
      (when b (setf *bookmarks* b) (save-bookmarks)))
    ;; ビューワへ反映: テーマ/背景/補間 + 本を開き直してキャッシュ等を作り直す。
    (%apply-theme)
    (%reopen vw)
    (%apply-background vw)
    (%apply-interpolation vw)
    t))

(defun %export-config (vw)
  (dotnet:static "Paalam.DialogHelper" "PickSaveConfig" (viewer-window vw) "paalam-config.lisp"
                 (dotnet:make-delegate "System.Action`1[System.String]"
                   (lambda (path)
                     (%atomic-write path (%config-bundle))
                     (%ui-post (lambda () (%status vw (format nil "設定をエクスポート → ~a" path))))))))

(defun %import-config (vw &optional after)
  "設定ファイルを選んで反映する。AFTER があれば反映後に呼ぶ (設定窓を閉じる等)。"
  (dotnet:static "Paalam.DialogHelper" "PickOpenConfig" (viewer-window vw)
                 (dotnet:make-delegate "System.Action`1[System.String]"
                   (lambda (path)
                     (let ((bundle (%read-sexp-file path)))
                       (%ui-post
                        (lambda ()
                          (cond ((and (consp bundle) (getf bundle :paalam-config))
                                 (%apply-config-bundle vw bundle)
                                 (%status vw "設定をインポートしました")
                                 (when after (funcall after)))
                                (t (%status vw "paalam の設定ファイルではありません"))))))))))

(defun %open-settings (vw)
  ;; 左に折りたたみツリー (カテゴリ)、右に該当ページ。選択で右を差し替える master/detail。
  ;; OK/キャンセルは全ページ共通 (コントロールは非表示でも値を保持するので OK で読める)。
  (let* ((owner (viewer-window vw))
         (swin  (dotnet:new "Avalonia.Controls.Window"))
         (dir-img-combo (%s-choice-combo *dir-choices* *image-direction*))   ; 画像/書庫の綴じ方向 (#33)
         (dir-pdf-combo (%s-choice-combo *dir-choices* *pdf-direction*))     ; PDF の綴じ方向 (#33)
         (cb-spread (%s-checkbox *spread*))
         (cb-fps    (%s-checkbox *first-page-single*))
         (cb-rec    (%s-checkbox *recursive*))
         (tb-mem    (%s-textbox *cache-memory-mb*))
         (tb-cache  (%s-textbox *cache-size*))
         (tb-pf     (%s-textbox *prefetch-ahead*))
         (tb-th     (%s-textbox *loader-threads*))
         (tb-slide  (%s-textbox *slideshow-interval*))
         (fit-combo    (%s-choice-combo *fit-choices* *fit-mode*))
         (bg-combo     (%s-choice-combo *bg-choices* *background-color* #'string=))
         (interp-combo (%s-choice-combo *interp-choices* *interpolation*))
         (theme-combo  (%s-choice-combo *theme-choices* *theme*))
         (exp-w-combo  (%s-choice-combo *export-width-choices* *export-width*))   ; 掃き出し最大幅 (#35 ②)
         (tb-dpi    (%s-textbox *pdf-dpi*))
         ;; キー割当の作業用コピー。(car kbox) が作業リスト。クロージャからは
         ;; (setf (car kbox) ..) で place を書き換える (let* 変数の再束縛は dotcl が不正 IL)。
         (kbox (list (let ((acc '()))
                       (dolist (c *commands*)
                         (dolist (k (%effective-keys c)) (push (cons k (command-id c)) acc)))
                       (nreverse acc))))
         (kb-stack (dotnet:new "Avalonia.Controls.StackPanel"))
         (content  (dotnet:new "Avalonia.Controls.ContentControl"))
         (win-p (dotnet:static "System.OperatingSystem" "IsWindows"))  ; 関連付けは Windows のみ (#11)
         (assoc-status (dotnet:new "Avalonia.Controls.TextBlock"))
         (cur (list "レイアウト"))   ; 現在のページ名 (検索クリア時の復帰先; #27)
         ;; 検索インデックス (項目ラベル . ページ名)。ページ構築 (spage) が各行を登録するので
         ;; 二重管理にならない。種類が特殊で %s-row を使わないページ (キー割当/関連付け) の
         ;; 別名だけ種として置く (ページ名そのものは do-search が pages から直接引く)。
         (sidx (list (list (cons "ショートカット" "キー割当") (cons "キーバインド" "キー割当")
                           (cons "拡張子" "関連付け") (cons "ファイル関連付け" "関連付け")))))
    (labels
        ((add-binding (k)
           ;; 数字 1-9 は count プレフィックス予約で割当てても発火しないので弾く (#23)。0 は可。
           (let ((d (%digit-key k)))
             (unless (or (and d (> d 0)) (assoc k (car kbox) :test #'string=))
               (setf (car kbox) (cons (cons k (command-id (car *commands*))) (car kbox)))))
           (refresh))
         (remove-binding (pair) (setf (car kbox) (remove pair (car kbox))) (refresh))
         (kb-row (pair)
           (let ((row  (dotnet:new "Avalonia.Controls.DockPanel"))
                 (klbl (dotnet:new "Avalonia.Controls.TextBlock"))
                 (cbox (dotnet:new "Avalonia.Controls.ComboBox"))
                 (bdel (dotnet:new "Avalonia.Controls.Button")))
             (dotnet:invoke klbl "set_Text" (%key-label (car pair)))
             (dotnet:invoke klbl "set_Width" 90d0)
             (dotnet:invoke klbl "set_VerticalAlignment"
                            (dotnet:static "Avalonia.Layout.VerticalAlignment" "Center"))
             (let ((items (dotnet:invoke cbox "get_Items")))
               (dolist (c *commands*) (dotnet:invoke items "Add" (command-label c))))
             (dotnet:invoke cbox "set_SelectedIndex"
                            (or (position (cdr pair) *commands* :key #'command-id) 0))
             (dotnet:add-event cbox "SelectionChanged"
               (lambda (s e) (declare (ignore s e))
                 (let ((i (dotnet:invoke cbox "get_SelectedIndex")))
                   (when (>= i 0) (setf (cdr pair) (command-id (nth i *commands*)))))))
             (dotnet:invoke bdel "set_Content" "×")
             (dotnet:invoke bdel "set_Margin" (dotnet:new "Avalonia.Thickness" 6d0 0d0 0d0 0d0))
             (dotnet:add-event bdel "Click"
               (lambda (s e) (declare (ignore s e)) (remove-binding pair)))
             (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" klbl
                            (dotnet:static "Avalonia.Controls.Dock" "Left"))
             (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" bdel
                            (dotnet:static "Avalonia.Controls.Dock" "Right"))
             (dotnet:invoke row "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 2d0))
             (let ((rc (dotnet:invoke row "get_Children")))
               (dotnet:invoke rc "Add" klbl)
               (dotnet:invoke rc "Add" bdel)
               (dotnet:invoke rc "Add" cbox))
             row))
         (refresh ()
           (let ((ch (dotnet:invoke kb-stack "get_Children")))
             (dotnet:invoke ch "Clear")
             (dolist (pair (sort (copy-list (car kbox)) #'string< :key #'car))
               (dotnet:invoke ch "Add" (kb-row pair)))))
         (page (rows)
           "rows を縦に並べた設定ページ (StackPanel)。"
           (let ((p (dotnet:new "Avalonia.Controls.StackPanel")))
             (dotnet:invoke p "set_Margin" (dotnet:new "Avalonia.Thickness" 16d0))
             (let ((ch (dotnet:invoke p "get_Children")))
               (dolist (r rows) (dotnet:invoke ch "Add" r)))
             p))
         (sd (key) (cdr (assoc key *setting-defaults*)))
         (reset-check (cb key) (lambda () (%set-check cb (sd key))))
         (reset-combo (combo choices key &optional (test #'eql))
           (lambda () (dotnet:invoke combo "set_SelectedIndex"
                                     (or (position (sd key) choices :key #'cdr :test test) 0))))
         (reset-text (tb key) (lambda () (dotnet:invoke tb "set_Text" (format nil "~a" (sd key)))))
         (spage (name rows)
           "NAME 見出し + (ラベル コントロール 既定化thunk) の行を並べた設定ページ。各ラベルを
検索インデックス sidx に登録 (ページ定義が検索の出どころ; #27)、末尾に「このページを既定に
戻す」(各 thunk を呼びコントロールを出荷時値へ; グローバルは OK まで触らない; #27)。"
           (let ((resets (mapcar #'third rows)))
             (page (append
                    (cons (%s-header name)
                          (mapcar (lambda (r)
                                    (push (cons (first r) name) (car sidx))
                                    (%s-row (first r) (second r)))
                                  rows))
                    (list (%s-button "このページを既定に戻す"
                                     (lambda () (dolist (f resets) (funcall f)))))))))
         (tvi (header &optional children)
           "TreeViewItem を作る (children があれば展開済みの親ノード)。"
           (let ((it (dotnet:new "Avalonia.Controls.TreeViewItem")))
             (dotnet:invoke it "set_Header" header)
             (when children
               (dotnet:invoke it "set_IsExpanded" t)
               (let ((items (dotnet:invoke it "get_Items")))
                 (dolist (c children) (dotnet:invoke items "Add" c))))
             it))
         (assoc-page ()
           "関連付けページ (#11): 拡張子ごとチェックボックス + 登録/登録解除 (Windows)。"
           (let ((wrap  (dotnet:new "Avalonia.Controls.WrapPanel"))
                 (boxes (mapcar (lambda (ext)
                                  (let ((cb (dotnet:new "Avalonia.Controls.CheckBox")))
                                    (dotnet:invoke cb "set_Content" (format nil ".~a" ext))
                                    (dotnet:invoke cb "set_Width" 92d0)
                                    (dotnet:invoke cb "set_IsThreeState" nil)
                                    (%set-check cb (and win-p (%assoc-registered-p ext)))
                                    (cons ext cb)))
                                *assoc-extensions*)))
             (dotnet:invoke wrap "set_Width" 380d0)
             (dolist (b boxes) (dotnet:invoke (dotnet:invoke wrap "get_Children") "Add" (cdr b)))
             (labels ((checked-exts ()
                        (let ((acc '()))
                          (dolist (b boxes) (when (dotnet:invoke (cdr b) "get_IsChecked") (push (car b) acc)))
                          (nreverse acc)))
                      (set-all (v) (dolist (b boxes) (%set-check (cdr b) v)))
                      (sync ()  ; レジストリの実状態をチェックに反映
                        (dolist (b boxes)
                          (%set-check (cdr b) (%assoc-registered-p (car b))))))
               (let ((row (dotnet:new "Avalonia.Controls.StackPanel")))
                 (dotnet:invoke row "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
                 (let ((rc (dotnet:invoke row "get_Children")))
                   (dotnet:invoke rc "Add" (%s-button "全選択" (lambda () (set-all t))))
                   (dotnet:invoke rc "Add" (%s-button "全解除" (lambda () (set-all nil))))
                   (dotnet:invoke rc "Add"
                     (%s-button "登録"
                       (lambda ()
                         (let ((es (checked-exts)))
                           (handler-case
                               (progn (%assoc-register es) (sync)
                                      (dotnet:invoke assoc-status "set_Text"
                                        (format nil "~d 件を「プログラムから開く」候補に登録しました。" (length es))))
                             (error (e) (dotnet:invoke assoc-status "set_Text" (format nil "失敗: ~a" e))))))))
                   (dotnet:invoke rc "Add"
                     (%s-button "登録解除"
                       (lambda ()
                         (handler-case
                             (progn (%assoc-unregister (checked-exts)) (sync)
                                    (dotnet:invoke assoc-status "set_Text" "選択分の関連付けを解除。"))
                           (error (e) (dotnet:invoke assoc-status "set_Text" (format nil "失敗: ~a" e))))))))
                 (let ((desc (dotnet:new "Avalonia.Controls.TextBlock")))
                   (dotnet:invoke desc "set_Text"
                     "関連付ける拡張子を選び「登録」(per-user・管理者権限不要)。候補に載るだけで既定は変わらないので、既定にしたい型は『既定のアプリ』で paalam を選ぶ。")
                   (dotnet:invoke desc "set_TextWrapping" (dotnet:static "Avalonia.Media.TextWrapping" "Wrap"))
                   (dotnet:invoke desc "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 0d0 8d0))
                   ;; 既定化は任意。登録時に自動で開かず、押したい人だけ Windows の設定へ。
                   (let ((defbtn (%s-button "既定のアプリ設定を開く…" (lambda () (%open-default-apps)))))
                     (dotnet:invoke defbtn "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 8d0 0d0 0d0))
                     (dotnet:invoke defbtn "set_HorizontalAlignment"
                                    (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Left"))
                     (page (list (%s-header "関連付け (Windows / 管理者権限不要)")
                                 desc wrap row defbtn assoc-status)))))))))
      (refresh)
      (let* ((scr (dotnet:new "Avalonia.Controls.ScrollViewer"))
             (addbtn (%s-button "＋ キーを追加"
                                (lambda () (%capture-key swin (lambda (k) (add-binding k))))))
             ;; 既定に戻す (#23): 作業リストを command-keys (出荷時既定) で作り直す。
             ;; OK で保存すると現行既定のスナップショットになる (= 新既定の取り込み手段)。
             (reset-btn (%s-button "既定に戻す"
                          (lambda ()
                            (setf (car kbox)
                                  (let ((acc '()))
                                    (dolist (c *commands*)
                                      (dolist (k (command-keys c))
                                        (push (cons k (command-id c)) acc)))
                                    (nreverse acc)))
                            (refresh))))
             (panel-layout
               (spage "レイアウト"
                      (list (list "綴じ方向 (画像/書庫)" dir-img-combo (reset-combo dir-img-combo *dir-choices* :image-direction))
                            (list "綴じ方向 (PDF)" dir-pdf-combo (reset-combo dir-pdf-combo *dir-choices* :pdf-direction))
                            (list "見開き" cb-spread (reset-check cb-spread :spread))
                            (list "表紙を単独表示" cb-fps (reset-check cb-fps :first-page-single))
                            (list "サブフォルダも再帰収集" cb-rec (reset-check cb-rec :recursive))
                            (list "フィット" fit-combo (reset-combo fit-combo *fit-choices* :fit-mode)))))
             (panel-quality
               (spage "画質"
                      (list (list "テーマ" theme-combo (reset-combo theme-combo *theme-choices* :theme))
                            (list "背景色" bg-combo (reset-combo bg-combo *bg-choices* :background-color #'string=))
                            (list "拡大補間" interp-combo (reset-combo interp-combo *interp-choices* :interpolation))
                            (list "PDF 解像度 (DPI)" tb-dpi (reset-text tb-dpi :pdf-dpi))
                            (list "掃き出し最大幅" exp-w-combo (reset-combo exp-w-combo *export-width-choices* :export-width)))))
             (panel-perf
               (spage "先読み"
                      (list (list "キャッシュ上限 (MB)" tb-mem (reset-text tb-mem :cache-memory-mb))
                            (list "キャッシュ最大ページ数" tb-cache (reset-text tb-cache :cache-size))
                            (list "先読み見開き数" tb-pf (reset-text tb-pf :prefetch-ahead))
                            (list "ローダースレッド数" tb-th (reset-text tb-th :loader-threads))
                            (list "スライドショー間隔 (秒)" tb-slide (reset-text tb-slide :slideshow-interval)))))
             (panel-keys
               (progn
                 (dotnet:invoke scr "set_MaxHeight" 360d0)
                 (dotnet:invoke scr "set_Content" kb-stack)
                 (page (list (%s-header "キー割当 (キー → 機能。1キー1機能・複数可。数字1-9はcount予約)")
                             scr addbtn reset-btn))))
             (panel-assoc (assoc-page))
             (panel-io
               (let ((row (dotnet:new "Avalonia.Controls.StackPanel"))
                     (desc (dotnet:new "Avalonia.Controls.TextBlock")))
                 (dotnet:invoke row "set_Orientation"
                                (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
                 (let ((rc (dotnet:invoke row "get_Children")))
                   (dotnet:invoke rc "Add" (%s-button "エクスポート…" (lambda () (%export-config vw))))
                   (dotnet:invoke rc "Add"
                     (%s-button "インポート…"
                                (lambda () (%import-config vw (lambda () (dotnet:invoke swin "Close")))))))
                 (dotnet:invoke desc "set_Text"
                   "設定すべて (レイアウト・画質・先読みの各設定 / キー割当 / メニュー構成 / マウスジェスチャ / しおり) を 1 ファイルに書き出し / 読み込み。環境間の持ち運び・共有・退避用。インポートは即時反映し、この設定窓を閉じます (未知項目は無視・欠けは現状維持)。")
                 (dotnet:invoke desc "set_TextWrapping" (dotnet:static "Avalonia.Media.TextWrapping" "Wrap"))
                 (dotnet:invoke desc "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 0d0 10d0))
                 (page (list (%s-header "設定の入出力") desc row))))
             (pages (append (list (cons "レイアウト" panel-layout)
                                  (cons "画質"      panel-quality)
                                  (cons "先読み"    panel-perf)
                                  (cons "キー割当"  panel-keys)
                                  (cons "入出力"    panel-io))
                            (when win-p (list (cons "関連付け" panel-assoc)))))
             (tree (dotnet:new "Avalonia.Controls.TreeView"))
             (leaf-layout  (tvi "レイアウト"))
             (leaf-quality (tvi "画質"))
             (leaf-perf    (tvi "先読み"))
             (leaf-keys    (tvi "キー割当"))
             (leaf-io      (tvi "入出力"))
             (leaf-assoc   (tvi "関連付け"))
             (g-view (tvi "表示" (list leaf-layout leaf-quality)))
             (g-perf (tvi "性能" (list leaf-perf)))
             (g-op   (tvi "操作" (list leaf-keys)))
             (g-system (tvi "システム" (if win-p (list leaf-io leaf-assoc) (list leaf-io))))
             (leaves (append (list (cons "レイアウト" leaf-layout) (cons "画質" leaf-quality)
                                   (cons "先読み" leaf-perf) (cons "キー割当" leaf-keys)
                                   (cons "入出力" leaf-io))
                             (when win-p (list (cons "関連付け" leaf-assoc)))))
             (sbox (dotnet:new "Avalonia.Controls.TextBox"))   ; 設定検索ボックス (#27)
             ;; name のページへ移動 (ツリー選択経由 → SelectionChanged が content を差し替え)。
             (nav (lambda (name)
                    (dotnet:invoke sbox "set_Text" "")        ; 検索をクリアして
                    (let ((lf (cdr (assoc name leaves :test #'equal))))
                      (when lf (dotnet:invoke lf "set_IsSelected" t)))))
             ;; 検索: 空ならカレントページ、非空ならヒット (ページ›項目) のボタン一覧を右に出す。
             (do-search
               (lambda (q)
                 (let ((qq (string-downcase (string-trim '(#\Space #\Tab) (or q "")))))
                   (if (zerop (length qq))
                       (dotnet:invoke content "set_Content"
                                      (cdr (assoc (car cur) pages :test #'equal)))
                       (let ((res (dotnet:new "Avalonia.Controls.StackPanel"))
                             (hits (remove-duplicates
                                    (append
                                     ;; ページ名そのもの (レイアウト/画質/…) もヒット対象。
                                     (loop for (nm . pg) in pages
                                           when (search qq (string-downcase nm))
                                           collect (cons nm nm))
                                     ;; 各ページが登録した項目ラベル + 特殊ページの別名。
                                     (loop for (label . page) in (car sidx)
                                           when (search qq (string-downcase label))
                                           collect (cons label page)))
                                    :test #'equal :from-end t)))
                         (dotnet:invoke res "set_Margin" (dotnet:new "Avalonia.Thickness" 16d0))
                         (if hits
                             (dolist (h hits)
                               (let* ((label (car h)) (page (cdr h))
                                      (btn (%s-button (format nil "~a 〉 ~a" page label)
                                                      (let ((pg page)) (lambda () (funcall nav pg))))))
                                 (dotnet:invoke btn "set_HorizontalAlignment"
                                                (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Left"))
                                 (dotnet:invoke btn "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 2d0))
                                 (dotnet:invoke (dotnet:invoke res "get_Children") "Add" btn)))
                             (let ((tb (dotnet:new "Avalonia.Controls.TextBlock")))
                               (dotnet:invoke tb "set_Text" "該当なし")
                               (dotnet:invoke (dotnet:invoke res "get_Children") "Add" tb)))
                         (dotnet:invoke content "set_Content" res))))))
             (root (dotnet:new "Avalonia.Controls.DockPanel"))
             (grid (dotnet:new "Avalonia.Controls.Grid"))
             (btns (dotnet:new "Avalonia.Controls.StackPanel")))
        ;; 左ツリー (折りたたみ可能なカテゴリ)。選択された葉の header からページを引いて右へ。
        (let ((items (dotnet:invoke tree "get_Items")))
          (dotnet:invoke items "Add" g-view)
          (dotnet:invoke items "Add" g-perf)
          (dotnet:invoke items "Add" g-op)
          (dotnet:invoke items "Add" g-system))   ; システム: 入出力 (+ 関連付けは Windows のみ)
        (dotnet:add-event tree "SelectionChanged"
          (lambda (s e) (declare (ignore s e))
            (let ((item (dotnet:invoke tree "get_SelectedItem")))
              (when item
                (let* ((name (dotnet:invoke item "get_Header"))
                       (p (cdr (assoc name pages :test #'equal))))
                  (when p (setf (car cur) name) (dotnet:invoke content "set_Content" p)))))))
        ;; 検索ボックス: 入力で横断ヒットをページ›項目で右に出す (#27)。
        (dotnet:invoke sbox "set_Watermark" "設定を検索…")
        (dotnet:invoke sbox "set_Margin" (dotnet:new "Avalonia.Thickness" 12d0 12d0 12d0 0d0))
        (dotnet:add-event sbox "TextChanged"
          (lambda (s e) (declare (ignore s e))
            (funcall do-search (dotnet:invoke sbox "get_Text"))))
        (dotnet:invoke content "set_Content" panel-layout)   ; 初期ページ
        (dotnet:invoke leaf-layout "set_IsSelected" t)
        ;; 2 列 grid: [tree(180) | content(*)]
        (let ((cols (dotnet:invoke grid "get_ColumnDefinitions"))
              (c0 (dotnet:new "Avalonia.Controls.ColumnDefinition")))
          (dotnet:invoke c0 "set_Width" (dotnet:new "Avalonia.Controls.GridLength" 180d0))
          (dotnet:invoke cols "Add" c0)
          (dotnet:invoke cols "Add" (dotnet:new "Avalonia.Controls.ColumnDefinition")))
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" tree 0)
        (dotnet:static "Avalonia.Controls.Grid" "SetColumn" content 1)
        (let ((gc (dotnet:invoke grid "get_Children")))
          (dotnet:invoke gc "Add" tree)
          (dotnet:invoke gc "Add" content))
        ;; OK/キャンセル (下部共通)
        (dotnet:invoke btns "set_Orientation"
                       (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
        (dotnet:invoke btns "set_HorizontalAlignment"
                       (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Right"))
        (dotnet:invoke btns "set_Margin" (dotnet:new "Avalonia.Thickness" 12d0))
        (let ((bc (dotnet:invoke btns "get_Children")))
          (dotnet:invoke bc "Add" (%s-button "キャンセル" (lambda () (dotnet:invoke swin "Close"))))
          (dotnet:invoke bc "Add"
            (%s-button "OK"
              (lambda ()
                (%settings-apply vw cb-spread cb-fps cb-rec tb-cache tb-pf tb-th
                                 fit-combo bg-combo interp-combo tb-dpi tb-mem tb-slide
                                 dir-img-combo dir-pdf-combo theme-combo exp-w-combo)
                ;; キー指向の作業リストを機能ごとにまとめ直して確定 (削除は空リストで反映)。
                (setf *keybinds*
                      (mapcar (lambda (c)
                                (let ((ks '()))
                                  (dolist (pair (car kbox))
                                    (when (eq (cdr pair) (command-id c)) (push (car pair) ks)))
                                  (cons (command-id c) (nreverse ks))))
                              *commands*))
                (rebuild-keymap)
                (save-keybinds)
                (dotnet:invoke swin "Close")))))
        ;; root: [sbox(上) | btns(下) | grid(残り)]
        (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" sbox
                       (dotnet:static "Avalonia.Controls.Dock" "Top"))
        (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" btns
                       (dotnet:static "Avalonia.Controls.Dock" "Bottom"))
        (let ((rc (dotnet:invoke root "get_Children")))
          (dotnet:invoke rc "Add" sbox)
          (dotnet:invoke rc "Add" btns)
          (dotnet:invoke rc "Add" grid))
        (dotnet:invoke swin "set_Title" "設定")
        (dotnet:invoke swin "set_Width" 640d0)
        (dotnet:invoke swin "set_Height" 520d0)
        (dotnet:invoke swin "set_WindowStartupLocation"
                       (dotnet:static "Avalonia.Controls.WindowStartupLocation" "CenterOwner"))
        (dotnet:invoke swin "set_Content" root)
        (dotnet:invoke swin "ShowDialog" owner)))))

