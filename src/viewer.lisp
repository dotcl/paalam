;;;; viewer.lisp — paalam (#39 main.lisp 分割): viewer 構造体・spread 表示・描画
(in-package :paalam)

(defstruct viewer book spreads (sindex 0) window left-img right-img source loader
           viewport scale-xf trans-xf (zoom 1d0) (tx 0d0) (ty 0d0)
           (fit-base 1d0)               ; フィット基準スケール (実効 = fit-base*zoom; #15)
           (content-w 0d0) (content-h 0d0)  ; 現在の見開きの content 寸法 (DIP; #15)
           (rotation 0) rot-xf          ; 回転角(度) と RotateTransform (#28)
           (slide-timer nil)            ; スライドショーの DispatcherTimer (nil=停止; #28)
           (loupe nil) (loupe-tx0 0d0) (loupe-ty0 0d0)  ; ルーペ状態とフィット時のパン基準 (#28)
           (loupe-cx 0d0) (loupe-cy 0d0)                ; ルーペの現在中心 (矢印キー移動用; #28)
           (dragging nil) (lx 0d0) (ly 0d0)
           clip-pane clip-stack         ; 素材ペイン (#18 Phase 2)
           clip-combo                   ; クリップ選択コンボ (#35 ③)
           clip-scroll                  ; 素材ペインの ScrollViewer (カーソル追従用; #35 ④)
           (clip-cursor 0)              ; クリップ編集の vi カーソル位置 (#35 ④)
           (clip-selected nil)          ; 複数選択 (index のリスト; #35 ④ Phase 2)
           (clip-pending nil)           ; vi 多キー待ち (:d=dd / :y=yy; #35 ④ Phase 2)
           (clip-count nil)             ; vi count プレフィックス (#35 ④ Phase 2)
           thumb-pane thumb-stack       ; ページサムネ本棚 (#6 A)
           browse-pane browse-tree      ; フォルダツリー (#6 B)
           (reorder-from nil)           ; ドラッグ並べ替え中の元 index (Phase 3)
           (reorder-img nil)            ; ドラッグ中のサムネ (不透明度を戻す用; #20)
           (reorder-line nil)           ; 挿入位置インジケータ Border (#20)
           (dnd-overlay nil)            ; DnD ドロップ可否ハイライト枠 (#20)
           (pending-count nil)          ; vi 風 count プレフィックス (数字キー蓄積; #23)
           (pending-g nil)              ; vi 風 多キー列 g 待ち (gg=先頭; #23)
           cmd-bar cmd-input cmd-list cmd-prompt         ; : コマンドバー / 検索バー (#23)
           (cmd-matches nil) (cmd-mode :command)         ; cmd-mode = :command / :search
           (search-matches nil) (search-index 0)         ; / 全文検索: ヒットページ index リスト (#23)
           (search-query nil) (search-hl nil)            ; 検索語 / ハイライト矩形 (overlay)
           (page-offsets nil)                            ; 現在見開きの page→(xoff yoff) (ハイライト配置用)
           (toc-entries nil) (toc-filtered nil)          ; 目次 (depth page title) 全件 / 絞り込み後 (#23)
           status-box status-text                        ; ウィンドウ内ステータス表示 (全画面でも見える; #23)
           menu                         ; メニューバー (#21; 全画面時に隠す)
           (gesturing nil) (gseq "") (glx 0d0) (gly 0d0)  ; マウスジェスチャ: 右ドラッグ中/方向列/直近点 (#29)
           (gesture-fired nil))         ; 直近の右リリースでジェスチャ発火 → ContextMenu 抑止 (#29)

(defun %reopen (vw)
  "source を開き直して book/loader/spreads を差し替える (再帰トグル等)。現在ページは維持。
source が nil の時 (空の窓 / クリッププレビュー中) は何もしない (#35 ⑤)。"
  (unless (viewer-source vw) (return-from %reopen))
  (let ((keep (%cur-page vw))
        (old-loader (viewer-loader vw)) (old-book (viewer-book vw)))
    (multiple-value-bind (book start) (open-book (viewer-source vw))
      (declare (ignore start))
      (setf *page-direction* (%book-direction book))   ; 種類で綴じ方向 (#33)
      (ignore-errors (loader-quiesce old-loader))   ; スレッドだけ止める (Bitmap は描画後に破棄)
      (setf (viewer-search-matches vw) nil (viewer-search-index vw) 0)  ; 本が変わったら検索結果は無効
      (%slideshow-stop vw)                       ; 本切替でスライドショー停止 (#28)
      (setf (viewer-rotation vw) 0)              ; 回転もリセット
      (setf (viewer-book vw) book
            (viewer-loader vw) (open-loader
                                (let ((p (viewer-source vw))) (lambda () (open-book p)))
                                (lambda (index)
                                  (%ui-post (lambda () (%on-page-loaded vw index)))))
            (viewer-spreads vw) (compute-spreads (book-page-count book))
            (viewer-sindex vw) (spread-index-of (viewer-spreads vw)
                                                (min keep (max 0 (1- (book-page-count book))))))
      (show-spread vw)
      (%thumb-pane-refresh vw)
      (%defer-free old-loader old-book))))

(defun open-path (vw path)
  "別の本 (DnD 等) を開いて差し替える。履歴があればそのページ、無ければ先頭から。
UI スレッドで呼ぶこと。"
  (handler-case
      (multiple-value-bind (book start) (open-book path)
        (setf *page-direction* (%book-direction book))   ; 種類で綴じ方向 (#33)
        (let ((old-loader (viewer-loader vw)) (old-book (viewer-book vw)))
          (ignore-errors (loader-quiesce old-loader))   ; スレッドだけ止める (Bitmap は描画後に破棄)
          (let* ((spreads (compute-spreads (book-page-count book)))
                 (resume (history-page path))
                 (page (if resume (min resume (max 0 (1- (book-page-count book)))) start)))
            (setf (viewer-source vw) path
                  (viewer-book vw) book
                  (viewer-loader vw) (open-loader
                                      (let ((p path)) (lambda () (open-book p)))
                                      (lambda (i) (%ui-post (lambda () (%on-page-loaded vw i)))))
                  (viewer-spreads vw) spreads
                  (viewer-sindex vw) (spread-index-of spreads page))
            (show-spread vw)
            (%thumb-pane-refresh vw)
            (%defer-free old-loader old-book))))
    (error (e) (log-err "open-path" e))))

(defun apply-transform (vw)
  ;; 変換順: 回転(content 中心まわり) → スケール(原点) → 平行移動。実効スケール = フィット基準 × zoom。
  (when (viewer-rot-xf vw)
    (dotnet:invoke (viewer-rot-xf vw) "set_Angle" (coerce (viewer-rotation vw) 'double-float))
    (dotnet:invoke (viewer-rot-xf vw) "set_CenterX" (/ (viewer-content-w vw) 2d0))
    (dotnet:invoke (viewer-rot-xf vw) "set_CenterY" (/ (viewer-content-h vw) 2d0)))
  (let ((s (* (viewer-fit-base vw) (viewer-zoom vw))))
    (dotnet:invoke (viewer-scale-xf vw) "set_ScaleX" s)
    (dotnet:invoke (viewer-scale-xf vw) "set_ScaleY" s))
  (dotnet:invoke (viewer-trans-xf vw) "set_X" (viewer-tx vw))
  (dotnet:invoke (viewer-trans-xf vw) "set_Y" (viewer-ty vw)))

(defun %apply-background (vw)
  "画像表示エリア (= フィット時の余白/レターボックス) の背景を *background-color* に (#2)。
ウィンドウ全体ではなくビューポートに塗る。こうするとメニュー等のクロムはテーマ既定のまま
なので、背景を白にしてもメニュー文字が読める。"
  (ignore-errors
    (dotnet:invoke (viewer-viewport vw) "set_Background"
                   (dotnet:static "Avalonia.Media.Brushes" *background-color*))))

(defun %theme-variant (kw)
  "テーマ keyword → Avalonia.Styling.ThemeVariant 静的値。:system=Default (OS 追従)。"
  (dotnet:static "Avalonia.Styling.ThemeVariant"
                 (case kw (:light "Light") (:dark "Dark") (t "Default"))))

(defun %apply-theme ()
  "*theme* を Application.RequestedThemeVariant に反映 (全ウィンドウのクロムに効く; #27)。
:system は Default = OS のライト/ダーク追従。"
  (ignore-errors
    (let ((app (dotnet:static "Avalonia.Application" "Current")))
      (when app
        (dotnet:invoke app "set_RequestedThemeVariant" (%theme-variant *theme*))))))

(defun %apply-interpolation (vw)
  "拡大補間品質を左右画像に設定 (#2)。:none=ニアレスト(ドット絵向け)/:low/:high。"
  (let ((mode (dotnet:static "Avalonia.Media.Imaging.BitmapInterpolationMode"
                             (case *interpolation*
                               (:none "None") (:low "LowQuality") (t "HighQuality")))))
    (dolist (img (list (viewer-left-img vw) (viewer-right-img vw)))
      (ignore-errors
        (dotnet:static "Avalonia.Media.RenderOptions" "SetBitmapInterpolationMode" img mode)))))

(defun %viewport-size (vw)
  "viewport の現在サイズ (DIP)。レイアウト前は 0。"
  (let ((b (dotnet:invoke (viewer-viewport vw) "get_Bounds")))
    (values (dotnet:invoke b "get_Width") (dotnet:invoke b "get_Height"))))

(defun %fit-base (cw ch vpw vph)
  "content (cw×ch) を viewport (vpw×vph) に合わせる基準スケール (モード別)。"
  (if (or (<= cw 0) (<= ch 0) (<= vpw 0) (<= vph 0))
      1d0
      (ecase *fit-mode*
        (:whole  (min (/ vpw cw) (/ vph ch)))
        (:width  (/ vpw cw))
        (:height (/ vph ch)))))

(defun %apply-fit (vw)
  "現在の content 寸法と viewport から基準スケールとアンカー (tx/ty) を決めて適用。
whole=中央寄せ、width/height=左上アンカー (はみ出しは pan でスクロール)。user zoom は保持。
回転 90/270 時は実効寸法を入れ替え、回転後 bbox (content 中心まわり) に合わせて配置 (#28)。"
  (multiple-value-bind (vpw vph) (%viewport-size vw)
    (let* ((cw (viewer-content-w vw)) (ch (viewer-content-h vw))
           (rot (mod (viewer-rotation vw) 360))
           (swap (or (= rot 90) (= rot 270)))
           (ew (if swap ch cw)) (eh (if swap cw ch))   ; 回転後の実効寸法
           (base (%fit-base ew eh vpw vph))
           (s (* base (viewer-zoom vw))))
      (setf (viewer-fit-base vw) base)
      ;; 回転後 bbox は content 中心 (cw/2,ch/2) を中心に ew×eh。その content 座標での左上 =
      ;; ((cw-ew)/2,(ch-eh)/2)。viewport 内の配置先 (余れば中央 / あふれれば先頭) へ平行移動。
      (let ((target-x (max 0d0 (/ (- vpw (* ew s)) 2d0)))
            (target-y (max 0d0 (/ (- vph (* eh s)) 2d0))))
        (setf (viewer-tx vw) (- target-x (* (/ (- cw ew) 2d0) s))
              (viewer-ty vw) (- target-y (* (/ (- ch eh) 2d0) s))))
      (apply-transform vw))))

(defun reset-view (vw)
  "ズームを等倍に戻し、現モードでフィットし直す。"
  (setf (viewer-zoom vw) 1d0)
  (%apply-fit vw))

(defun compute-spreads (n)
  "n ページを *spread* / *first-page-single* に従って見開き単位へ分割。"
  (let ((spreads '()) (i 0))
    (cond
      ((<= n 0))
      ((not *spread*)
       (loop for k from 0 below n do (push (list k) spreads)))
      (t
       (when *first-page-single*
         (push (list 0) spreads) (setf i 1))
       (loop while (< i n) do
         (if (< (1+ i) n)
             (progn (push (list i (1+ i)) spreads) (incf i 2))
             (progn (push (list i) spreads) (incf i))))))
    (coerce (nreverse spreads) 'vector)))

(defun spread-index-of (spreads page)
  "page を含む見開きの index (見つからなければ 0)。"
  (or (position-if (lambda (pages) (member page pages)) spreads) 0))

(defun %bmp-size (bmp)
  "Bitmap の DIP サイズ (values w h)。"
  (let ((s (dotnet:invoke bmp "get_Size")))
    (values (dotnet:invoke s "get_Width") (dotnet:invoke s "get_Height"))))

(defun %canvas-place (img bmp x y)
  "img に bmp を設定し、Canvas 上の (x,y) にネイティブ寸法で配置 (bmp=nil は非表示)。"
  (dotnet:invoke img "set_Source" bmp)
  (dotnet:invoke img "set_IsVisible" (and bmp t))
  (when bmp
    (dotnet:static "Avalonia.Controls.Canvas" "SetLeft" img (coerce x 'double-float))
    (dotnet:static "Avalonia.Controls.Canvas" "SetTop"  img (coerce y 'double-float))))

(defun %fit-label ()
  (case *fit-mode* (:width " [幅]") (:height " [高]") (t "")))

(defun %spread-title (vw pages)
  (let* ((book (viewer-book vw)) (n (book-page-count book))
         (path (viewer-source vw))
         (star (if (some (lambda (p) (bookmarked-p path p)) pages) "★ " ""))
         ;; #35 ③: カレントクリップ名と枚数 (複数クリップ or 非空のとき)。
         (clip (if (or *clip* (> (length *clips*) 1))
                   (format nil " [素材 ~a:~d]" (clip-current-name) (length *clip*)) ""))
         (fit  (%fit-label)))
    (if (= (length pages) 1)
        (format nil "~apaalam — ~a (~d/~d)~a~a"
                star (book-page-name book (first pages)) (1+ (first pages)) n clip fit)
        (destructuring-bind (lo hi) pages
          (format nil "~apaalam — ~a｜~a (~d-~d/~d)~a~a"
                  star (book-page-name book lo) (book-page-name book hi)
                  (1+ lo) (1+ hi) n clip fit)))))

(defun %page-bitmap (vw index) (and index (loader-get (viewer-loader vw) index)))

(defun %render-spread (vw)
  "現在の見開きを cache から Canvas に配置し content 寸法を更新 → フィット適用。
未ロードは空欄 (後で %on-page-loaded が埋める)。見開きは綴じ方向で左右を突き合わせ、
縦は背の高い方に中央寄せ。content 幅 = 左右幅の和、高さ = 高い方 (#15)。"
  (let* ((spreads (viewer-spreads vw)) (m (length spreads)))
    (when (plusp m)
      (let* ((si (viewer-sindex vw))
             (pages (aref spreads si))
             (li (viewer-left-img vw))
             (ri (viewer-right-img vw)))
        (%clear-highlights vw)                ; ページが変わるので前の検索ハイライトを消す
        (%status-clear vw)                    ; ステータスメッセージもページ移動で消す
        (setf (viewer-page-offsets vw) nil)
        (if (= (length pages) 1)
            (let ((bmp (%page-bitmap vw (first pages))))
              (multiple-value-bind (w h) (if bmp (%bmp-size bmp) (values 0d0 0d0))
                (%canvas-place li bmp 0d0 0d0)
                (dotnet:invoke ri "set_IsVisible" nil)
                (setf (viewer-content-w vw) w (viewer-content-h vw) h)
                (setf (viewer-page-offsets vw) (list (list (first pages) 0d0 0d0 w h)))))
            (destructuring-bind (lo hi) pages
              ;; rtl(右開き): 下位 index=lo を右に、hi を左に。ltr は逆。
              (let* ((left-page  (if (eq *page-direction* :rtl) hi lo))
                     (right-page (if (eq *page-direction* :rtl) lo hi))
                     (lb (%page-bitmap vw left-page))
                     (rb (%page-bitmap vw right-page)))
                (multiple-value-bind (lw lh) (if lb (%bmp-size lb) (values 0d0 0d0))
                  (multiple-value-bind (rw rh) (if rb (%bmp-size rb) (values 0d0 0d0))
                    (let ((ch (max lh rh)))
                      (%canvas-place li lb 0d0 (/ (- ch lh) 2d0))
                      (%canvas-place ri rb lw  (/ (- ch rh) 2d0))
                      (setf (viewer-content-w vw) (+ lw rw)
                            (viewer-content-h vw) ch)
                      (setf (viewer-page-offsets vw)
                            (list (list left-page  0d0 (/ (- ch lh) 2d0) lw lh)
                                  (list right-page lw  (/ (- ch rh) 2d0) rw rh)))))))))
        (%apply-fit vw)))))

(defun %update-title (vw)
  ;; タイトルは本文情報のみ。検索件数等のメッセージは %status (ウィンドウ内) に出す
  ;; (全画面ではタイトルバーが見えないため)。
  (let ((spreads (viewer-spreads vw)))
    (when (plusp (length spreads))
      (dotnet:invoke (viewer-window vw) "set_Title"
                     (%spread-title vw (aref spreads (viewer-sindex vw)))))))

(defun %goto-bookmark (vw dir)
  "現在の本のブックマークのうち、現在ページから dir 方向の最寄りページへ。"
  (let* ((cur (%cur-page vw))
         (pages (book-bookmark-pages (viewer-source vw)))
         (target (if (> dir 0)
                     (find-if (lambda (p) (> p cur)) pages)
                     (find-if (lambda (p) (< p cur)) (reverse pages)))))
    (when target
      (setf (viewer-sindex vw) (spread-index-of (viewer-spreads vw) target))
      (show-spread vw))))

(defun %load-order (vw)
  "現在の見開き + 双方向 prefetch の page index を優先順で (#25)。
順序は 現在 → +1 → -1 → +2..+ahead → -2..-behind の前方優先。behind=min(2,ahead)。"
  (let* ((spreads (viewer-spreads vw)) (m (length spreads)) (si (viewer-sindex vw))
         (ahead (max 0 *prefetch-ahead*)) (behind (min 2 ahead))
         (ks '()))
    (when (plusp m)
      (push 0 ks)
      (when (>= ahead 1) (push 1 ks))
      (when (>= behind 1) (push -1 ks))
      (loop for k from 2 to ahead do (push k ks))
      (loop for k from 2 to behind do (push (- k) ks))
      (let ((acc '()))
        (dolist (k (nreverse ks))                       ; [0 1 -1 2..ahead -2..-behind]
          (let ((s (+ si k)))
            (when (and (>= s 0) (< s m))
              (dolist (p (aref spreads s)) (push p acc)))))
        (nreverse acc)))))

(defun show-spread (vw)
  "見開き表示 (sindex クランプ)。必要ページをローダーに要求し cache から描画、
読み方向の次見開きも prefetch 要求する。"
  (setf (viewer-loupe vw) nil)          ; ページ移動でルーペは解除 (#28)
  (let* ((spreads (viewer-spreads vw)) (m (length spreads)))
    (when (plusp m)
      (let ((si (max 0 (min (viewer-sindex vw) (1- m)))))
        (setf (viewer-sindex vw) si)
        ;; LRU 中心は *ページ* index (見開き index ではない — 追い出し距離が狂うため)。
        (loader-request (viewer-loader vw) (%load-order vw) (%cur-page vw))
        (%dbg "show si=~a pages=~a hit=~a" si (aref spreads si)
              (mapcar (lambda (p) (if (%page-bitmap vw p) 1 0)) (aref spreads si)))
        (%render-spread vw)
        (%update-title vw)
        (history-update (viewer-source vw) (%cur-page vw))   ; in-memory。保存は終了時。
        (reset-view vw)))))

(defun %on-page-loaded (vw index)
  "ワーカーが index をロード完了 → 現在の見開きに含まれるなら cache から再描画
(zoom/pan は維持; reset-view しない)。"
  (let* ((spreads (viewer-spreads vw)) (m (length spreads)))
    (when (plusp m)
      (let ((pages (aref spreads (viewer-sindex vw))))
        (when (member index pages) (%render-spread vw))))))

(defun %recompute (vw keep-page)
  "spreads を計算し直し、keep-page を含む見開きへ位置を合わせて再描画。"
  (setf (viewer-spreads vw) (compute-spreads (book-page-count (viewer-book vw))))
  (setf (viewer-sindex vw) (spread-index-of (viewer-spreads vw) keep-page))
  (show-spread vw))

(defun %cur-page (vw)
  (let ((sp (viewer-spreads vw)))
    (if (plusp (length sp)) (first (aref sp (viewer-sindex vw))) 0)))

(defparameter *zoom-step* 1.2d0 "ホイール 1 ノッチあたりの倍率。")
(defparameter *zoom-min* 0.1d0)
(defparameter *zoom-max* 10d0)

(defun %pos-xy (e elem)
  "PointerEventArgs の elem 基準座標を (values x y) で。"
  (let ((p (dotnet:invoke e "GetPosition" elem)))
    (values (dotnet:invoke p "get_X") (dotnet:invoke p "get_Y"))))

(defun %on-wheel (vw e)
  "ホイールズーム。カーソル位置を固定点にして拡大縮小する。"
  (let* ((dy (dotnet:invoke (dotnet:invoke e "get_Delta") "get_Y"))
         (s  (viewer-zoom vw))
         (s2 (max *zoom-min* (min *zoom-max* (* s (if (> dy 0) *zoom-step* (/ 1d0 *zoom-step*)))))))
    (multiple-value-bind (cx cy) (%pos-xy e (viewer-viewport vw))
      ;; t' = c - (s'/s)(c - t)  (origin top-left, screen = s*p + t)
      (let ((r (/ s2 s)))
        (setf (viewer-tx vw) (- cx (* r (- cx (viewer-tx vw))))
              (viewer-ty vw) (- cy (* r (- cy (viewer-ty vw))))
              (viewer-zoom vw) s2)))
    (apply-transform vw)))

(defun %left-pressed-p (vw e)
  "今この瞬間、左ボタンが押されているか (PointerPoint.Properties)。"
  (dotnet:invoke (dotnet:invoke (dotnet:invoke e "GetCurrentPoint" (viewer-viewport vw))
                                "get_Properties")
                 "get_IsLeftButtonPressed"))

(defun %right-pressed-p (vw e)
  "今この瞬間、右ボタンが押されているか (PointerPoint.Properties)。"
  (dotnet:invoke (dotnet:invoke (dotnet:invoke e "GetCurrentPoint" (viewer-viewport vw))
                                "get_Properties")
                 "get_IsRightButtonPressed"))

;;; マウスジェスチャ (#29): 右ボタンドラッグの方向列 (U/D/L/R, 例 "DL") → コマンド。
;;; 純粋な右クリック (移動なし) は従来どおりコンテキストメニュー。発火時のみ ContextMenu 抑止。
(defparameter *gesture-threshold* 28d0 "方向を1つ確定する最小移動量 (px)。")
(defparameter *default-gestures*
  '(("L" . :reading-fwd) ("R" . :reading-bwd)
    ("U" . :first-spread) ("D" . :last-spread))
  "出荷時のマウスジェスチャ (右ドラッグ方向列 → コマンド id)。左右は読み進む/戻る
(綴じ方向に追従)、上下は先頭/末尾。")
(defparameter *gestures* (copy-tree *default-gestures*)
  "現在のマウスジェスチャ割当 ((方向列 . command-id) ...)。gestures.lisp に永続化。")
(defun load-gestures ()
  (let ((d (%read-state "gestures.lisp"))) (when (and (listp d) d) (setf *gestures* d))))
(defun save-gestures () (%write-state "gestures.lisp" *gestures*))

(defun %gesture-start (vw e)
  (multiple-value-bind (x y) (%pos-xy e (viewer-viewport vw))
    (setf (viewer-gesturing vw) t (viewer-gseq vw) ""
          (viewer-glx vw) x (viewer-gly vw) y
          (viewer-gesture-fired vw) nil)))

(defun %gesture-step (seq lx ly x y)
  "方向列 SEQ と直近点 (LX,LY) に点 (X,Y) を取り込む。閾値以上動いていれば方向
(U/D/L/R) を判定し、直前と違えば追記する。(values seq lx ly moved-p) を返す
(moved-p=t のとき lx,ly は x,y に更新)。ビューワ本体と編集UIの記録ボックスで共用。"
  (let ((dx (- x lx)) (dy (- y ly)))
    (if (>= (max (abs dx) (abs dy)) *gesture-threshold*)
        (let* ((dir (if (> (abs dx) (abs dy))
                        (if (> dx 0d0) "R" "L")
                        (if (> dy 0d0) "D" "U")))
               (seq2 (if (or (zerop (length seq))
                             (not (char= (char seq (1- (length seq))) (char dir 0))))
                         (concatenate 'string seq dir) seq)))
          (values seq2 x y t))
        (values seq lx ly nil))))

(defun %gesture-move (vw e)
  (multiple-value-bind (x y) (%pos-xy e (viewer-viewport vw))
    (multiple-value-bind (seq lx ly moved)
        (%gesture-step (viewer-gseq vw) (viewer-glx vw) (viewer-gly vw) x y)
      (when moved
        ;; 動いた時点で「ジェスチャ発火」を立てておく (リリースより先に ContextMenu の
        ;; Opening が走っても確実に抑止できるように; #29)。
        (setf (viewer-gseq vw) seq (viewer-glx vw) lx (viewer-gly vw) ly
              (viewer-gesture-fired vw) t)))))

(defun %gesture-end (vw)
  "右ドラッグ終了。方向列があればコマンド実行し、コンテキストメニューを抑止する。"
  (when (viewer-gesturing vw)
    (setf (viewer-gesturing vw) nil)
    (let ((seq (viewer-gseq vw)))
      (when (plusp (length seq))
        (setf (viewer-gesture-fired vw) t)   ; ContextRequested 抑止フラグ
        (%status vw (format nil "ジェスチャ: ~a" seq))
        (let ((id (cdr (assoc seq *gestures* :test #'string-equal))))
          (when id (%run-command vw id)))))))

(defun %on-press (vw e)
  ;; パン開始は左ボタンのみ。メニュー等の押下で誤ってドラッグ状態にしない。
  ;; ルーペ中はパンしない (移動はカーソル追従が担う)。
  (cond
    ((and (not (viewer-loupe vw)) (%left-pressed-p vw e))
     (multiple-value-bind (x y) (%pos-xy e (viewer-viewport vw))
       (setf (viewer-dragging vw) t (viewer-lx vw) x (viewer-ly vw) y)))
    ((%right-pressed-p vw e)            ; 右ドラッグ = ジェスチャ開始 (#29)
     (%gesture-start vw e))))

(defun %on-move (vw e)
  (when (viewer-gesturing vw)           ; ジェスチャ中は方向を蓄積 (#29)
    (if (%right-pressed-p vw e)
        (progn (%gesture-move vw e) (return-from %on-move))
        (%gesture-end vw)))             ; release 取りこぼしの自己修復
  (when (viewer-loupe vw) (%loupe-follow vw e) (return-from %on-move))
  (when (viewer-dragging vw)
    ;; release を取りこぼして dragging が居残っても、ボタンが離れていれば自己修復で停止
    ;; (メニュー/別ウィンドウのダイアログで PointerReleased が main に返らない場合の対策)。
    (if (%left-pressed-p vw e)
        (multiple-value-bind (x y) (%pos-xy e (viewer-viewport vw))
          (incf (viewer-tx vw) (- x (viewer-lx vw)))
          (incf (viewer-ty vw) (- y (viewer-ly vw)))
          (setf (viewer-lx vw) x (viewer-ly vw) y)
          (apply-transform vw))
        (setf (viewer-dragging vw) nil))))

(defun %on-release (vw e) (declare (ignore e))
  (%gesture-end vw)                     ; 右ドラッグ終了でジェスチャ確定 (#29)
  (setf (viewer-dragging vw) nil))

(defun %fullscreen-p (vw)
  (string= (dotnet:invoke (dotnet:invoke (viewer-window vw) "get_WindowState") "ToString")
           "FullScreen"))

(defun %set-fullscreen (vw on)
  (dotnet:invoke (viewer-window vw) "set_WindowState"
                 (dotnet:static "Avalonia.Controls.WindowState" (if on "FullScreen" "Normal")))
  ;; 全画面では没入のためメニューバーを隠す (DockPanel が root を全面に広げる)。
  (when (viewer-menu vw)
    (dotnet:invoke (viewer-menu vw) "set_IsVisible" (not on))))

(defun %quit (vw) (dotnet:invoke (viewer-window vw) "Close"))

;;; 回転 (#28)。stage の RotateTransform を回し、%apply-fit が 90/270 で寸法を入れ替えて配置。
(defun %rotate (vw deg)
  (setf (viewer-rotation vw) (mod (+ (viewer-rotation vw) deg) 360))
  (%apply-fit vw)
  (%status vw (format nil "回転 ~d°" (viewer-rotation vw))))
(defun %rotate-right (vw) (%rotate vw 90))
(defun %rotate-left  (vw) (%rotate vw -90))

;;; スライドショー (#28)。DispatcherTimer で自動送り、末尾で停止。
(defun %slideshow-stop (vw)
  (when (viewer-slide-timer vw)
    (ignore-errors (dotnet:invoke (viewer-slide-timer vw) "Stop"))
    (setf (viewer-slide-timer vw) nil)))
(defun %slideshow-toggle (vw)
  "スライドショーの開始/停止。間隔 *slideshow-interval* 秒で次の見開きへ、末尾で停止。"
  (if (viewer-slide-timer vw)
      (progn (%slideshow-stop vw) (%status vw "スライドショー停止"))
      (let ((timer (dotnet:new "Avalonia.Threading.DispatcherTimer")))
        (dotnet:invoke timer "set_Interval"
                       (dotnet:static "System.TimeSpan" "FromSeconds"
                                      (coerce (max 1 *slideshow-interval*) 'double-float)))
        (dotnet:add-event timer "Tick"
          (lambda (s e) (declare (ignore s e))
            (let ((m (length (viewer-spreads vw))) (si (viewer-sindex vw)))
              (if (< si (1- m)) (%next-spread vw) (%slideshow-stop vw)))))
        (dotnet:invoke timer "Start")
        (setf (viewer-slide-timer vw) timer)
        (%status vw (format nil "スライドショー (~d秒間隔)" *slideshow-interval*)))))

;;; ルーペ (拡大鏡) (#28)。フルビュー型: カーソル位置の content を *loupe-scale* で拡大し、
;;; カーソル移動に追従。フィット時のパン基準 (loupe-tx0/ty0) からカーソル固定式で再計算する。
(defun %loupe-apply (vw cx cy)
  "ビューポート座標 (cx,cy) のフィット時 content をその場で *loupe-scale* 倍に拡大配置。"
  (let ((f *loupe-scale*))
    (setf (viewer-loupe-cx vw) cx (viewer-loupe-cy vw) cy
          (viewer-zoom vw) f
          (viewer-tx vw) (- cx (* (- cx (viewer-loupe-tx0 vw)) f))
          (viewer-ty vw) (- cy (* (- cy (viewer-loupe-ty0 vw)) f)))
    (apply-transform vw)))

(defun %loupe-nudge (vw dx dy)
  "ルーペ中心を矢印キーで動かす (ページ送りの代わり; #28)。"
  (%loupe-apply vw (+ (viewer-loupe-cx vw) dx) (+ (viewer-loupe-cy vw) dy)))

(defun %loupe-follow (vw e)
  (multiple-value-bind (cx cy) (%pos-xy e (viewer-viewport vw)) (%loupe-apply vw cx cy)))

(defun %loupe-toggle (vw)
  "ルーペ ON/OFF。ON 時はフィット(zoom1)に戻してパン基準を確定 → 中央で拡大開始。"
  (if (viewer-loupe vw)
      (progn (setf (viewer-loupe vw) nil) (reset-view vw) (%status vw "ルーペ OFF"))
      (progn
        (reset-view vw)                                  ; フィットに戻して tx/ty を確定
        (setf (viewer-loupe-tx0 vw) (viewer-tx vw)
              (viewer-loupe-ty0 vw) (viewer-ty vw)
              (viewer-loupe vw) t)
        (multiple-value-bind (vpw vph) (%viewport-size vw)
          (%loupe-apply vw (/ vpw 2d0) (/ vph 2d0)))     ; 初期は中央で拡大
        (%status vw (format nil "ルーペ ON (~,1fx) — マウス/矢印で移動 / Z で OFF" *loupe-scale*)))))

