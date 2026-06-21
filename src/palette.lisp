;;;; palette.lisp — paalam (#39 main.lisp 分割): コマンドパレット/検索/目次/cmdbar/status (#23)
(in-package :paalam)

;;; : コマンドパレット (#23)。*commands* を名前/ラベルで絞り込んで実行。
(defun %palette-name (c)
  "パレットでタイプする識別子 (id を小文字化; 例 :next-spread → \"next-spread\")。"
  (string-downcase (symbol-name (command-id c))))

(defun %palette-matches (query)
  "QUERY にマッチするコマンドをランク順 (完全一致→前方一致→部分一致→ラベル一致) で返す。
空なら *commands* 全部。同ランク内は *commands* の元順を保つ。"
  (let ((q (string-downcase (string-trim ": " query))))   ; 先頭の : / 空白は無視
    (if (string= q "")
        (copy-list *commands*)
        (let ((scored '()))
          (dolist (c *commands*)
            (let* ((name (%palette-name c))
                   (rank (cond ((string= name q) 0)
                               ((eql 0 (search q name)) 1)
                               ((search q name) 2)
                               ((search q (command-label c)) 3)
                               (t nil))))
              (when rank (push (cons rank c) scored))))
          (mapcar #'cdr (stable-sort (nreverse scored) #'< :key #'car))))))

;;; / 全文検索 (#23)。pdf は本文 (PdfText/FPDFText)、他はページ名で。n/N でヒット間を巡回。
(defun %array->list (arr)
  (when arr
    (loop for i from 0 below (dotnet:invoke arr "get_Length")
          collect (dotnet:invoke arr "GetValue" i))))

(defgeneric book-search (book query)
  (:documentation "QUERY を含むページの 0-based index リスト (出現順)。"))
(defmethod book-search ((b pdf-book) query)
  (%array->list (dotnet:invoke (pdfb-doc b) "SearchPages" query)))
(defmethod book-search ((b t) query)
  ;; pdf 以外はページ名 (ファイル名) の部分一致 (大小無視)。
  (let ((q (string-downcase query)) (acc '()))
    (dotimes (i (book-page-count b))
      (when (search q (string-downcase (or (book-page-name b i) "")))
        (push i acc)))
    (nreverse acc)))

;;; 目次 (TOC) (#23)。pdf の bookmark を (depth page title) リストに。他形式は無し。
(defun %parse-outline (s)
  "PdfText.Outline の \"depth<TAB>page<TAB>title\" 行文字列を (depth page title) リストへ。"
  (when (and s (plusp (length s)))
    (let ((entries '()))
      (with-input-from-string (in s)
        (loop for line = (read-line in nil nil) while line do
          (let* ((t1 (position #\Tab line))
                 (t2 (and t1 (position #\Tab line :start (1+ t1)))))
            (when t2
              (let ((depth (ignore-errors (parse-integer line :end t1)))
                    (page  (ignore-errors (parse-integer line :start (1+ t1) :end t2)))
                    (title (subseq line (1+ t2))))
                (when (and depth page)
                  (push (list depth page title) entries)))))))
      (nreverse entries))))

(defgeneric book-outline (book)
  (:documentation "目次 (depth page title) のリスト。無ければ nil。"))
(defmethod book-outline ((b pdf-book)) (%parse-outline (dotnet:invoke (pdfb-doc b) "Outline")))
(defmethod book-outline ((b t)) nil)

(defun %run-search (vw query)
  "QUERY で現在の本を検索。ヒットがあれば先頭へジャンプ (件数はタイトルに出る)。"
  (let* ((book (viewer-book vw))
         ;; バーを開いた / (や : ) が先頭に入り込むので除去 (vi の /word 習慣にも合う)。
         (q (string-trim '(#\Space #\/ #\:) (or query "")))
         (ms (and book (plusp (length q)) (ignore-errors (book-search book q)))))
    (setf (viewer-search-matches vw) ms (viewer-search-index vw) 0 (viewer-search-query vw) q)
    (if ms
        (progn (%jump-to-page vw (first ms))   ; jump→render が status をクリア→下で出し直す
               (%highlight-match vw)
               (%status vw (format nil "検索 1/~d 件  n/N で移動" (length ms))))
        (when (plusp (length q))
          (%status vw (format nil "「~a」 見つかりません" q))))))

(defun %search-step (vw dir)
  "検索ヒット間を dir (+1/-1) 方向に循環してジャンプ (n/N)。"
  (let ((ms (viewer-search-matches vw)))
    (when ms
      (setf (viewer-search-index vw) (mod (+ (viewer-search-index vw) dir) (length ms)))
      (%jump-to-page vw (nth (viewer-search-index vw) ms))
      (%highlight-match vw)
      (%status vw (format nil "検索 ~d/~d 件  n/N で移動" (1+ (viewer-search-index vw)) (length ms))))))

(defun %clear-highlights (vw)
  "重ねた検索ハイライト矩形を stage canvas から除去。"
  (let ((hl (viewer-search-hl vw)))
    (when hl
      (let ((canvas (ignore-errors (dotnet:invoke (viewer-left-img vw) "get_Parent"))))
        (when canvas
          (let ((children (dotnet:invoke canvas "get_Children")))
            (dolist (r hl) (ignore-errors (dotnet:invoke children "Remove" r))))))
      (setf (viewer-search-hl vw) nil))))

(defun %highlight-match (vw)
  "現在のヒットページの一致矩形 (正規化) をページ表示寸法で展開し、stage に半透明黄で重ねる。
pdf-book のみ (page-name 検索の他形式は矩形なし)。"
  (%clear-highlights vw)
  (let ((ms (viewer-search-matches vw))
        (book (viewer-book vw))
        (q (viewer-search-query vw)))
    (when (and ms q (typep book 'pdf-book))
      (let* ((page (nth (viewer-search-index vw) ms))
             (lay (assoc page (viewer-page-offsets vw)))
             (canvas (ignore-errors (dotnet:invoke (viewer-left-img vw) "get_Parent"))))
        (when (and lay canvas)
          (destructuring-bind (xoff yoff w h) (cdr lay)
            (let ((coords (%array->list (dotnet:invoke (pdfb-doc book) "MatchRects" page q)))
                  (children (dotnet:invoke canvas "get_Children")))
              (loop for (rx ry rw rh) on coords by #'cddddr while rh do
                (let ((rect (dotnet:new "Avalonia.Controls.Shapes.Rectangle")))
                  (dotnet:invoke rect "set_Width"  (coerce (* rw w) 'double-float))
                  (dotnet:invoke rect "set_Height" (coerce (* rh h) 'double-float))
                  (dotnet:invoke rect "set_Fill" (dotnet:static "Avalonia.Media.Brushes" "Yellow"))
                  (dotnet:invoke rect "set_Opacity" 0.5d0)
                  (dotnet:invoke rect "set_IsHitTestVisible" nil)
                  (dotnet:static "Avalonia.Controls.Canvas" "SetLeft" rect (coerce (+ xoff (* rx w)) 'double-float))
                  (dotnet:static "Avalonia.Controls.Canvas" "SetTop"  rect (coerce (+ yoff (* ry h)) 'double-float))
                  (dotnet:invoke children "Add" rect)
                  (push rect (viewer-search-hl vw)))))))))))

(defun %palette-row (c)
  "候補1行: 「ラベル  名前  [キー]」(タイプすべき名前が分かるように)。"
  (let ((k (%key-label (%command-primary-key (command-id c)))))
    (format nil "~a   ~a~@[   [~a]~]" (command-label c) (%palette-name c) k)))

(defun %cmd-populate (vw)
  "入力に応じて候補リストを更新 (先頭を選択)。"
  (let* ((tb  (viewer-cmd-input vw))
         (lb  (viewer-cmd-list vw))
         (raw (or (dotnet:invoke tb "get_Text") "")))
    ;; : で開いた直後にその : がフィールドへ入り込む場合がある。先頭の : を除去して
    ;; 表示と検索を整える (vi の :q 習慣にも合う)。除去後は : 無しなので再 TextChanged で再帰しない。
    (when (and (plusp (length raw)) (char= (char raw 0) #\:))
      (setf raw (string-left-trim ":" raw))
      (dotnet:invoke tb "set_Text" raw)
      (ignore-errors (dotnet:invoke tb "set_CaretIndex" (length raw))))
    (let ((ms (%palette-matches raw)))
      (setf (viewer-cmd-matches vw) ms)
      (let ((items (dotnet:invoke lb "get_Items")))
        (dotnet:invoke items "Clear")
        (dolist (c ms) (dotnet:invoke items "Add" (%palette-row c))))
      (dotnet:invoke lb "set_IsVisible" (and ms t))
      (when ms (dotnet:invoke lb "set_SelectedIndex" 0)))))

(defun %cmd-strip-leading (vw ch)
  "入力欄の先頭に紛れ込んだプロンプト文字 CH (: や /) を除去して表示を整える。"
  (let* ((tb (viewer-cmd-input vw))
         (raw (or (dotnet:invoke tb "get_Text") "")))
    (when (and (plusp (length raw)) (char= (char raw 0) ch))
      (let ((s (string-left-trim (string ch) raw)))
        (dotnet:invoke tb "set_Text" s)
        (ignore-errors (dotnet:invoke tb "set_CaretIndex" (length s)))))))

(defun %cmd-move (vw d)
  ;; リストの実項目数で巡回 (モード非依存: command/toc どちらの一覧でも正しく動く)。
  (let* ((lb (viewer-cmd-list vw))
         (n  (dotnet:invoke (dotnet:invoke lb "get_Items") "get_Count")))
    (when (plusp n)
      (dotnet:invoke lb "set_SelectedIndex"
                     (mod (+ (max 0 (dotnet:invoke lb "get_SelectedIndex")) d) n)))))

(defun %cmd-hide (vw)
  "コマンドバーを閉じてビューポートにフォーカスを戻す。"
  (dotnet:invoke (viewer-cmd-bar vw) "set_IsVisible" nil)
  (%focus-viewport vw))

(defun %cmd-run (vw)
  (let* ((lb (viewer-cmd-list vw))
         (i  (dotnet:invoke lb "get_SelectedIndex"))
         (c  (and (>= i 0) (nth i (viewer-cmd-matches vw)))))
    (%cmd-hide vw)
    (when c (%run-command vw (command-id c)))))

(defun %command-palette (vw)
  "vi 風 : コマンドバーを開く (#23)。入力で絞り込み、↑↓/クリック選択、Enter 実行、Esc 閉じる。
別窓ではなく本体下部のオーバーレイ。バー表示中は %on-key が無効化され入力がコマンド誤発火しない。"
  (setf (viewer-cmd-mode vw) :command)
  (let ((tb (viewer-cmd-input vw)))
    (when (viewer-cmd-prompt vw) (dotnet:invoke (viewer-cmd-prompt vw) "set_Text" ":"))
    (dotnet:invoke (viewer-cmd-list vw) "set_IsVisible" t)
    (dotnet:invoke (viewer-cmd-bar vw) "set_IsVisible" t)
    (dotnet:invoke tb "set_Text" "")
    (%cmd-populate vw)
    ;; フォーカスは1サイクル遅延 (開いたキー : 自身が入力欄に入るのを防ぐ)。
    (%ui-post (lambda () (dotnet:invoke tb "Focus")))))

(defun %command-search (vw)
  "vi 風 / 検索バーを開く (#23)。Enter で本文/ページ名検索→先頭ヒットへ。以後 n/N で巡回。"
  (setf (viewer-cmd-mode vw) :search)
  (let ((tb (viewer-cmd-input vw)))
    (when (viewer-cmd-prompt vw) (dotnet:invoke (viewer-cmd-prompt vw) "set_Text" "/"))
    (dotnet:invoke (viewer-cmd-list vw) "set_IsVisible" nil)  ; 検索はリスト不要 (n/N で巡回)
    (dotnet:invoke (viewer-cmd-bar vw) "set_IsVisible" t)
    (dotnet:invoke tb "set_Text" "")
    ;; フォーカスは1サイクル遅延 (開いたキー / 自身が入力欄に入るのを防ぐ)。
    (%ui-post (lambda () (dotnet:invoke tb "Focus")))))

(defun %toc-row (e)
  "目次1行: 深さインデント + タイトル + ページ。e = (depth page title)。"
  (destructuring-bind (depth page title) e
    (format nil "~a~a   p~d" (make-string (* 2 (max 0 depth)) :initial-element #\Space)
            title (1+ page))))

(defun %toc-populate (vw)
  "入力で目次をタイトル部分一致 (大小無視) 絞り込みしてリスト更新。"
  (let* ((tb (viewer-cmd-input vw))
         (lb (viewer-cmd-list vw))
         (q  (string-downcase (string-trim " " (or (dotnet:invoke tb "get_Text") ""))))
         (es (if (string= q "")
                 (viewer-toc-entries vw)
                 (remove-if-not (lambda (e) (search q (string-downcase (third e))))
                                (viewer-toc-entries vw)))))
    (setf (viewer-toc-filtered vw) es)
    (let ((items (dotnet:invoke lb "get_Items")))
      (dotnet:invoke items "Clear")
      (dolist (e es) (dotnet:invoke items "Add" (%toc-row e))))
    (dotnet:invoke lb "set_IsVisible" (and es t))
    (when es (dotnet:invoke lb "set_SelectedIndex" 0))))

(defun %toc-run (vw)
  "選択中の目次項目のページへジャンプ。"
  (let* ((lb (viewer-cmd-list vw))
         (i  (dotnet:invoke lb "get_SelectedIndex"))
         (e  (and (>= i 0) (nth i (viewer-toc-filtered vw)))))
    (%cmd-hide vw)
    (when e
      (let ((page (second e)))
        (when (and page (>= page 0)) (%jump-to-page vw page))))))

(defun %command-toc (vw)
  "目次バーを開く (#23)。入力でタイトル絞り込み、Enter/クリックでそのページへ。目次が無ければ通知。"
  (let ((es (ignore-errors (book-outline (viewer-book vw)))))
    (if (null es)
        (%status vw "目次なし (この本にはブックマークがありません)")
        (progn
          (setf (viewer-cmd-mode vw) :toc (viewer-toc-entries vw) es)
          (let ((tb (viewer-cmd-input vw)))
            (when (viewer-cmd-prompt vw) (dotnet:invoke (viewer-cmd-prompt vw) "set_Text" "≡"))
            (dotnet:invoke (viewer-cmd-list vw) "set_IsVisible" t)
            (dotnet:invoke (viewer-cmd-bar vw) "set_IsVisible" t)
            (dotnet:invoke tb "set_Text" "")
            (%toc-populate vw)
            ;; フォーカスは1サイクル遅延 (開いたキー O 自身が入力欄に入るのを防ぐ)。
            (%ui-post (lambda () (dotnet:invoke tb "Focus"))))))))

(defun %cmd-accept (vw)
  "コマンドバー確定 (Enter / ダブルクリック)。モードで分岐。"
  (ecase (viewer-cmd-mode vw)
    (:command (%cmd-run vw))
    (:search  (let ((q (or (dotnet:invoke (viewer-cmd-input vw) "get_Text") "")))
                (%cmd-hide vw) (%run-search vw q)))
    (:toc     (%toc-run vw))))

(defun %make-cmdbar (vw)
  "本体下部に貼る vi 風コマンドバー (既定非表示)。controls を viewer に保持して返す。"
  (let* ((bar (dotnet:new "Avalonia.Controls.Border"))
         (col (dotnet:new "Avalonia.Controls.StackPanel"))
         (lb  (dotnet:new "Avalonia.Controls.ListBox"))
         (row (dotnet:new "Avalonia.Controls.StackPanel"))
         (lbl (dotnet:new "Avalonia.Controls.TextBlock"))
         (tb  (dotnet:new "Avalonia.Controls.TextBox")))
    (dotnet:invoke bar "set_IsVisible" nil)
    ;; 全幅にせず中央下に寄せた小さめのボックス (content に合わせて伸縮)。
    (dotnet:invoke bar "set_VerticalAlignment"
                   (dotnet:static "Avalonia.Layout.VerticalAlignment" "Bottom"))
    (dotnet:invoke bar "set_HorizontalAlignment"
                   (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Center"))
    (dotnet:invoke bar "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 0d0 24d0))
    (dotnet:invoke bar "set_Padding" (dotnet:new "Avalonia.Thickness" 8d0 6d0 8d0 6d0))
    (dotnet:invoke bar "set_CornerRadius" (dotnet:new "Avalonia.CornerRadius" 6d0))
    (dotnet:invoke bar "set_Background" (dotnet:static "Avalonia.Media.Brushes" "Black"))
    (dotnet:invoke bar "set_Opacity" 0.92d0)
    (dotnet:invoke lb "set_MaxHeight" 220d0)
    (dotnet:invoke lb "set_Background" (dotnet:static "Avalonia.Media.Brushes" "Transparent"))
    ;; 候補と入力欄の間に余白 (最下段の候補が入力に被って見えるのを防ぐ)。
    (dotnet:invoke lb "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 0d0 8d0))
    (dotnet:invoke row "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
    (dotnet:invoke lbl "set_Text" ":")
    (dotnet:invoke lbl "set_Foreground" (dotnet:static "Avalonia.Media.Brushes" "White"))
    (dotnet:invoke lbl "set_VerticalAlignment"
                   (dotnet:static "Avalonia.Layout.VerticalAlignment" "Center"))
    (dotnet:invoke lbl "set_Margin" (dotnet:new "Avalonia.Thickness" 8d0 0d0 4d0 0d0))
    (dotnet:invoke tb "set_BorderThickness" (dotnet:new "Avalonia.Thickness" 0d0))
    (dotnet:invoke tb "set_Background" (dotnet:static "Avalonia.Media.Brushes" "Transparent"))
    (dotnet:invoke tb "set_Foreground" (dotnet:static "Avalonia.Media.Brushes" "White"))
    (dotnet:invoke tb "set_MinWidth" 320d0)
    (dotnet:invoke tb "set_Watermark" "コマンド名 (Enter 実行 / Esc 閉じる)")
    ;; ライブ絞り込み: command=コマンド / toc=目次。search はライブ検索せず先頭 / を除去のみ。
    (dotnet:add-event tb "TextChanged"
      (lambda (s e) (declare (ignore s e))
        (ecase (viewer-cmd-mode vw)
          (:command (%cmd-populate vw))      ; 候補絞り込み (先頭 : も除去)
          (:toc     (%toc-populate vw))      ; 目次絞り込み
          (:search  (%cmd-strip-leading vw #\/)))))  ; 検索: 先頭 / を除去 (表示を整える)
    ;; キーは TextBox 自身で捌く (Window に付けると Enter がバブルしないため)。
    (dotnet:add-event tb "KeyDown"
      (lambda (s e) (declare (ignore s))
        (let ((kn (dotnet:invoke (dotnet:invoke e "get_Key") "ToString")))
          ;; Avalonia は Enter/Return が同値で ToString は "Return"。両方受ける。
          (cond ((or (string= kn "Return") (string= kn "Enter"))
                 (dotnet:invoke e "set_Handled" t) (%cmd-accept vw))  ; モードで分岐
                ((string= kn "Escape") (dotnet:invoke e "set_Handled" t) (%cmd-hide vw))
                ;; Ctrl-n/Ctrl-p で項目移動 (vi 寄り; 単一行 TextBox が ↑↓ を食う問題も回避)。↑↓ も一応残す。
                ((and (string= kn "N") (%ctrl? e)) (dotnet:invoke e "set_Handled" t) (%cmd-move vw 1))
                ((and (string= kn "P") (%ctrl? e)) (dotnet:invoke e "set_Handled" t) (%cmd-move vw -1))
                ((string= kn "Down")   (dotnet:invoke e "set_Handled" t) (%cmd-move vw 1))
                ((string= kn "Up")     (dotnet:invoke e "set_Handled" t) (%cmd-move vw -1))))))
    (dotnet:add-event lb "DoubleTapped" (lambda (s e) (declare (ignore s e)) (%cmd-accept vw)))
    (let ((rc (dotnet:invoke row "get_Children")))
      (dotnet:invoke rc "Add" lbl)
      (dotnet:invoke rc "Add" tb))
    (let ((cc (dotnet:invoke col "get_Children")))
      (dotnet:invoke cc "Add" lb)
      (dotnet:invoke cc "Add" row))
    (dotnet:invoke bar "set_Child" col)
    (setf (viewer-cmd-bar vw) bar
          (viewer-cmd-input vw) tb
          (viewer-cmd-list vw) lb
          (viewer-cmd-prompt vw) lbl)
    bar))

(defun %make-status (vw)
  "本体上部中央に出す小さなステータス表示 (検索件数/メッセージ; 全画面でも見える)。既定非表示。"
  (let ((box (dotnet:new "Avalonia.Controls.Border"))
        (txt (dotnet:new "Avalonia.Controls.TextBlock")))
    (dotnet:invoke box "set_IsVisible" nil)
    (dotnet:invoke box "set_VerticalAlignment" (dotnet:static "Avalonia.Layout.VerticalAlignment" "Top"))
    (dotnet:invoke box "set_HorizontalAlignment" (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Center"))
    (dotnet:invoke box "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 12d0 0d0 0d0))
    (dotnet:invoke box "set_Padding" (dotnet:new "Avalonia.Thickness" 12d0 6d0 12d0 6d0))
    (dotnet:invoke box "set_CornerRadius" (dotnet:new "Avalonia.CornerRadius" 6d0))
    (dotnet:invoke box "set_Background" (dotnet:static "Avalonia.Media.Brushes" "Black"))
    (dotnet:invoke box "set_Opacity" 0.8d0)
    (dotnet:invoke box "set_IsHitTestVisible" nil)
    (dotnet:invoke txt "set_Foreground" (dotnet:static "Avalonia.Media.Brushes" "White"))
    (dotnet:invoke box "set_Child" txt)
    (setf (viewer-status-box vw) box (viewer-status-text vw) txt)
    box))

(defun %status (vw text)
  "ステータス表示に TEXT を出す (タイトルバーが無い全画面でも見える)。次のページ描画で消える。"
  (let ((box (viewer-status-box vw)) (txt (viewer-status-text vw)))
    (when (and box txt)
      (dotnet:invoke txt "set_Text" text)
      (dotnet:invoke box "set_IsVisible" t))))

(defun %status-clear (vw)
  (let ((box (viewer-status-box vw)))
    (when box (dotnet:invoke box "set_IsVisible" nil))))

