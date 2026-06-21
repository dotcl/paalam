;;;; command.lisp — paalam (#39 main.lisp 分割): コマンドテーブル/キーマップ/メニュー構成 (#10)
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; コマンドテーブル (#10): キー操作とメニューの単一の出どころ。
;;; 各コマンドは id・ラベル (メニュー表示)・既定キー (Avalonia Key.ToString 名)・
;;; ハンドラ (vw を取る関数 or シンボル) を持つ。キー dispatch もメニュー構築も
;;; ここから派生させ、ショートカット表記の二重管理を無くす。将来のキーバインド
;;; 編集 UI (段階2) はこのテーブルと *keymap* を編集する形にする。
;;; ------------------------------------------------------------------
(defstruct (command (:constructor make-command (id label keys handler)))
  id label keys handler)

;;; コマンド用のハンドラ (いずれも vw を 1 つ取る)。
(defun %next-spread (vw) (incf (viewer-sindex vw)) (show-spread vw))
(defun %prev-spread (vw) (decf (viewer-sindex vw)) (show-spread vw))
(defun %first-spread (vw) (setf (viewer-sindex vw) 0) (show-spread vw))
(defun %last-spread (vw)
  (setf (viewer-sindex vw) (1- (length (viewer-spreads vw)))) (show-spread vw))
(defun %reading-forward (vw)    ; ← : 綴じ方向に沿って読み進む
  (if (eq *page-direction* :rtl) (%next-spread vw) (%prev-spread vw)))
(defun %reading-backward (vw)   ; → : 読み戻る
  (if (eq *page-direction* :rtl) (%prev-spread vw) (%next-spread vw)))
(defun %toggle-fullscreen (vw) (%set-fullscreen vw (not (%fullscreen-p vw))))
(defun %escape (vw) (if (%fullscreen-p vw) (%set-fullscreen vw nil) (%quit vw)))
(defun %toggle-bookmark-current (vw)
  (toggle-bookmark (viewer-source vw) (%cur-page vw)) (%update-title vw))
(defun %clip-add-current (vw)
  "現在の見開きの全ページを素材に追加 (見開きは両ページ)。"
  (let ((sp (viewer-spreads vw)))
    (when (plusp (length sp))
      (dolist (p (aref sp (viewer-sindex vw))) (clip-add (viewer-source vw) p))))
  (%clip-cache-warm)                   ; #35 ①: 追加分を背景でローカルコピー
  (%update-title vw)
  (when (dotnet:invoke (viewer-clip-pane vw) "get_IsVisible") (%rebuild-clip-pane vw)))

(defparameter *commands*
  ;; ラベルにキーは書かない (メニューが %keymap から付ける)。keys は既定割当。
  (list
   (make-command :open-file     "開く (ファイル)…" '()        (lambda (vw) (%open-file-dialog vw (viewer-window vw))))
   (make-command :open-folder   "開く (フォルダ)…" '()        (lambda (vw) (%open-folder-dialog vw (viewer-window vw))))
   (make-command :settings      "設定…"            '()        (lambda (vw) (%open-settings vw)))
   (make-command :edit-menu     "メニュー編集…"    '()        '%open-menu-editor)
   (make-command :edit-gestures "マウスジェスチャ編集…" '()    '%open-gesture-editor)
   (make-command :quit          "終了"             '("Q")     '%quit)
   (make-command :next-spread   "次の見開き"       '("PageDown" "Next" "Space" "J") '%next-spread)
   (make-command :prev-spread   "前の見開き"       '("PageUp" "Prior" "Back" "K")   '%prev-spread)
   (make-command :reading-fwd   "読み進む"         '("Left")  '%reading-forward)
   (make-command :reading-bwd   "読み戻る"         '("Right") '%reading-backward)
   (make-command :first-spread  "先頭へ"           '("Home")  '%first-spread)
   (make-command :last-spread   "末尾へ"           '("End")   '%last-spread)
   (make-command :fullscreen    "全画面"           '("F11" "F") '%toggle-fullscreen)
   (make-command :spread        "見開き"           '("S")     '%toggle-spread)
   (make-command :direction     "綴じ方向"         '("D")     '%toggle-direction)
   (make-command :recursive     "再帰収集"         '("R")     '%toggle-recursive)
   (make-command :fit           "フィット"         '("D0" "NumPad0") 'reset-view)
   (make-command :fit-mode      "フィット切替"     '("W")     '%cycle-fit-mode)
   (make-command :rotate-right  "右回転"           '("OemPeriod") '%rotate-right)
   (make-command :rotate-left   "左回転"           '("OemComma")  '%rotate-left)
   (make-command :slideshow     "スライドショー"   '("F5")    '%slideshow-toggle)
   (make-command :loupe         "ルーペ (拡大鏡)"  '("Z")     '%loupe-toggle)
   (make-command :goto          "ページ移動 (gg=先頭 / G=末尾 / [count]g)" '("G") '%goto-page)
   (make-command :palette       "コマンドパレット" '("OemSemicolon") '%command-palette)
   (make-command :search        "検索 (/)"          '("OemQuestion" "Divide") '%command-search)
   (make-command :toc           "目次 (O)"          '("O")     '%command-toc)
   (make-command :clip-pane     "素材ペイン"       '("P")     '%toggle-clip-pane)
   (make-command :thumb-pane    "本棚 (サムネ)"    '("T")     '%toggle-thumb-pane)
   (make-command :browse-pane   "フォルダ"         '("L")     '%toggle-browse-pane)
   (make-command :clip-add      "素材に追加"       '("A")     '%clip-add-current)
   (make-command :export        "掃き出し…"        '("E")     '%pane-export)
   (make-command :bookmark      "しおり切替"       '("B")     '%toggle-bookmark-current)
   (make-command :prev-bookmark "前のしおり"       '("OemOpenBrackets")  (lambda (vw) (%goto-bookmark vw -1)))
   (make-command :next-bookmark "次のしおり"       '("OemCloseBrackets") (lambda (vw) (%goto-bookmark vw  1)))
   (make-command :escape        "戻る / 終了"      '("Escape") '%escape))
  "全コマンド (定義順)。メニュー/キーバインド編集はこの順序を参照する。")

(defun %command (id) (find id *commands* :key #'command-id))

;;; メニュー構成 (#26): メニュー/コンテキストメニューの単一の出どころ。
;;; (group-key  メニュータイトル  コマンドid...) の順序付きリスト。ここを編集すれば
;;; メニューが変わる (手書きの %build-menu との二重管理を解消)。未掲載のコマンド
;;; (reading-fwd/bwd, escape 等の内部操作) はメニューに出さない。
(defparameter *command-menu*
  '((:file "ファイル" :open-file :open-folder :settings :edit-menu :edit-gestures :quit)
    (:view "表示"    :fullscreen :fit :fit-mode :rotate-left :rotate-right :loupe :slideshow
                     :spread :direction :recursive
                     :browse-pane :thumb-pane :clip-pane)
    (:jump "移動"    :first-spread :prev-spread :next-spread :last-spread :goto
                     :prev-bookmark :next-bookmark)
    (:tool "ツール"  :palette :search :toc)
    (:clip "素材"    :clip-add :export :bookmark))
  "メニュー構成。group ごとに submenu を作り該当コマンドを並べる。ユーザ編集で
menu.lisp に永続化され差し替わる (#26)。")
(defparameter *default-command-menu* (copy-tree *command-menu*)
  "出荷時のメニュー構成 (編集の「既定に戻す」用)。")

;;; キー割当 (#10 段階2): ユーザ上書きを *keybinds* に持ち、無いコマンドは既定キー。
;;; *keymap* (キー名→id) はそこから構築。keymap.lisp に永続化 (設定ウィンドウで編集)。
(defparameter *keybinds* nil
  "ユーザ上書き ((command-id . (key ...)) ...)。エントリの無いコマンドは既定キー。")
(defparameter *keymap* (make-hash-table :test 'equal)
  "キー名 (Avalonia Key.ToString) → コマンド id。*keybinds*+既定から構築。")

(defun %effective-keys (c)
  "コマンド C の実効キー (上書きがあればそれ、無ければ既定)。"
  (let ((o (assoc (command-id c) *keybinds*)))
    (if o (cdr o) (command-keys c))))

(defun rebuild-keymap ()
  "*keybinds*+既定から *keymap* を作り直す。"
  (clrhash *keymap*)
  (dolist (c *commands*)
    (dolist (k (%effective-keys c)) (setf (gethash k *keymap*) (command-id c)))))
(rebuild-keymap)

(defun load-keybinds ()
  (let ((d (%read-state "keymap.lisp"))) (when (listp d) (setf *keybinds* d)))
  (rebuild-keymap))
(defun save-keybinds () (%write-state "keymap.lisp" *keybinds*))

;;; メニュー構成の永続化 (#26)。menu.lisp があれば *command-menu* を差し替え。
(defun load-menu ()
  (let ((d (%read-state "menu.lisp")))
    (when (and (consp d) (consp (car d))) (setf *command-menu* d))))
(defun save-menu () (%write-state "menu.lisp" *command-menu*))

(defun %command-primary-key (id)
  "ID の代表キー (メニュー表示用; 既定キー順を優先、無ければ keymap から拾う)。"
  (let ((c (%command id)))
    (or (and c (find-if (lambda (k) (eq (gethash k *keymap*) id)) (command-keys c)))
        (loop for k being the hash-keys of *keymap* using (hash-value v)
              when (eq v id) return k))))

(defun %run-command (vw id)
  (let ((c (%command id)))
    (when c (funcall (command-handler c) vw))))

(defun %digit-key (kn)
  "キー名 (Avalonia Key.ToString) が数字なら 0-9、でなければ nil。D0-D9 / NumPad0-9。"
  (cond ((and (= (length kn) 2) (char= (char kn 0) #\D)) (digit-char-p (char kn 1)))
        ((and (= (length kn) 7) (string= (subseq kn 0 6) "NumPad")) (digit-char-p (char kn 6)))
        (t nil)))

(defun %shift? (e)
  "KeyEventArgs に Shift 修飾が乗っているか (vi の g/G 判別用; #23)。
KeyModifiers は大小キーで同じ Key.G になるため修飾で見分ける。"
  (and (search "Shift"
               (dotnet:invoke (dotnet:invoke e "get_KeyModifiers") "ToString"))
       t))

(defun %ctrl? (e)
  "KeyEventArgs に Control 修飾が乗っているか (バー内の Ctrl-n/p 等)。"
  (and (search "Control"
               (dotnet:invoke (dotnet:invoke e "get_KeyModifiers") "ToString"))
       t))

(defun %on-key (vw e)
  "Window KeyDown。vi 風入力 (#23): 数字=count 蓄積、g/G=先頭/末尾/移動の小状態機械、
他は *keymap* を引いてコマンド実行 (count はコマンドが viewer-pending-count で読む)。
0 単独は count を始めない (既存の 0=フィットを活かす; count 継続中の 0 のみ桁送り)。
g/G: [count]g=count ページ目 / gg=先頭 / G(Shift)=末尾 / g 単独=次の g 待ち。
コマンドバー表示中はここでは何もしない (入力は TextBox 側で捌く; コマンド誤発火防止)。"
  (when (and (viewer-cmd-bar vw)
             (dotnet:invoke (viewer-cmd-bar vw) "get_IsVisible"))
    (return-from %on-key))
  (let* ((kn (dotnet:invoke (dotnet:invoke e "get_Key") "ToString"))
         (d  (%digit-key kn)))
    (cond
      ;; ルーペ中の矢印キーはルーペ中心を動かす (ページ送りに化けるのを防ぐ; #28)。
      ((and (viewer-loupe vw) (member kn '("Left" "Right" "Up" "Down") :test #'string=))
       (let ((step 60d0))
         (cond ((string= kn "Left")  (%loupe-nudge vw (- step) 0d0))
               ((string= kn "Right") (%loupe-nudge vw step 0d0))
               ((string= kn "Up")    (%loupe-nudge vw 0d0 (- step)))
               ((string= kn "Down")  (%loupe-nudge vw 0d0 step)))))
      ;; g / G — Avalonia では大小とも Key.G。Shift で末尾、count で移動、二連で先頭。
      ((string= kn "G")
       (let ((cnt   (viewer-pending-count vw))
             (shift (%shift? e))
             (was-g (viewer-pending-g vw)))
         (cond (cnt   (%goto-page vw))      ; [count]g / [count]G → count ページ目
               (shift (%last-spread vw))    ; G → 末尾
               (was-g (%first-spread vw))   ; gg → 先頭
               (t     (setf (viewer-pending-g vw) t))) ; g 単独 → 次キー待ち
         ;; 「待ち」分岐 (count/shift/前回g すべて無) のときだけ pending-g を残す。
         (unless (and (not cnt) (not shift) (not was-g))
           (setf (viewer-pending-g vw) nil))
         (setf (viewer-pending-count vw) nil)))
      ;; n / N — 検索ヒット間の巡回 (検索中のみ; n=次, N=Shift=前)。非検索時は通常キーマップへ落ちる。
      ((and (string= kn "N") (viewer-search-matches vw))
       (%search-step vw (if (%shift? e) -1 1))
       (setf (viewer-pending-count vw) nil (viewer-pending-g vw) nil))
      ;; 数字: count 蓄積
      ((and d (or (> d 0) (viewer-pending-count vw)))
       (setf (viewer-pending-g vw) nil
             (viewer-pending-count vw) (+ (* 10 (or (viewer-pending-count vw) 0)) d)))
      ;; その他: 通常コマンド
      (t
       (setf (viewer-pending-g vw) nil)
       (let ((id (gethash kn *keymap*)))
         (when id (%run-command vw id))     ; count は実行中に pending-count で読まれる
         (setf (viewer-pending-count vw) nil))))))

