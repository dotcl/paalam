;;;; clip.lisp — paalam (#39 main.lisp 分割): 素材クリップ (#18/#35)
(in-package :paalam)

;;; 素材クリップ (#18 Phase 1): ((source . page) ...) 追加順。zip 掃き出し用。
;;; #35 ③: 名前付き複数クリップ。*clips* がレジストリ、*clip* はカレントの中身
;;; (既存コードは *clip* をそのまま使い、save-clip がカレントへ書き戻す最小チャーン設計)。
(defparameter *clip* nil "カレントクリップの中身 ((source . page) ...)。*clips* のカレント要素と同期。")
(defparameter *clips* (list (cons "既定" nil))
  "名前付きクリップのレジストリ ((名前 . items) ...)。#35 ③")
(defparameter *current-clip* 0 "*clips* 内のカレントインデックス。")
(defun clip-current-cell () (nth (max 0 (min *current-clip* (1- (length *clips*)))) *clips*))
(defun clip-current-name () (car (clip-current-cell)))
(defun %clip-sync () "カレント *clip* を *clips* のカレント要素へ書き戻す。"
  (setf (cdr (clip-current-cell)) *clip*))
;; サムネのメモリキャッシュ (source.page -> Bitmap)。ペインを開くたびに source から
;; 再取得しないため (NAS で効く)。loader の cache とは別物 (loader は evict で Dispose
;; するので共有しない)。
(defparameter *thumb-cache* (make-hash-table :test 'equal))
(defun %thumb-get (src page) (gethash (cons src page) *thumb-cache*))
(defun %thumb-put (src page bmp) (setf (gethash (cons src page) *thumb-cache*) bmp))
(defun save-clip ()
  "レジストリ全体 + カレント index を clips.lisp に保存 (カレント *clip* を先に同期; #35 ③)。"
  (%clip-sync)
  (%write-state "clips.lisp" (cons *current-clip* *clips*)))
(defun load-clip ()
  "clips.lisp 優先。無ければ旧 clip.lisp を既定クリップへ移行する (#35 ③)。"
  (let ((d (%read-state "clips.lisp")))
    (if (and (consp d) (integerp (car d)) (consp (cdr d)) (every #'consp (cdr d)))
        (setf *current-clip* (car d) *clips* (cdr d))
        (let ((old (%read-state "clip.lisp")))           ; 旧単一クリップ → 既定クリップ
          (setf *clips* (list (cons "既定" (and (listp old) old)))
                *current-clip* 0))))
  (unless (and (consp *clips*) (every #'consp *clips*)) (setf *clips* (list (cons "既定" nil))))
  (setf *current-clip* (max 0 (min *current-clip* (1- (length *clips*))))
        *clip* (cdr (clip-current-cell))))
(defun clip-add (source page)
  "素材に (source . page) を追加 (重複はスキップ)。"
  (when (and source
             (not (member-if (lambda (e) (and (string-equal (car e) source) (eql (cdr e) page)))
                             *clip*)))
    (setf *clip* (append *clip* (list (cons source page))))
    (save-clip)))
(defun clip-clear ()
  ;; カレントクリップを空に。参照されなくなったキャッシュのみ掃除 (他クリップ分は残す; #35 ③)。
  (setf *clip* nil) (clrhash *thumb-cache*) (save-clip) (%clip-cache-gc))

;;; クリップのローカルキャッシュ (#35 ①)。クリップしたページの原本バイトを state 配下
;;; clip-cache/ に1回だけコピーし、掃き出しは NAS を再読みせずローカルから詰める。
;;; マニフェスト *clip-cache*: (source . page) -> (相対ファイル名 ページ名)。clip-cache.lisp に永続化。
;;; 既存 clip.lisp (キャッシュ無し) は掃き出し時に source 読みへフォールバックするので互換。
(defparameter *clip-cache* (make-hash-table :test 'equal))
(defun %clip-cache-dir () (%state-file "clip-cache/"))
(defun %fnv1a-hex (string)
  "FNV-1a 64bit を 16 進に (キャッシュのファイル名用; .NET crypto 非依存。衝突は実用上無視可)。"
  (let ((h 14695981039346656037))
    (loop for ch across string
          do (setf h (logand (* (logxor h (char-code ch)) 1099511628211) #xFFFFFFFFFFFFFFFF)))
    (format nil "~16,'0x" h)))
(defun load-clip-cache ()
  (clrhash *clip-cache*)
  (let ((d (%read-state "clip-cache.lisp")))
    (when (listp d)
      (dolist (e d) (when (and (consp e) (consp (car e)))
                      (setf (gethash (car e) *clip-cache*) (cdr e)))))))
(defun save-clip-cache ()
  (let (al) (maphash (lambda (k v) (push (cons k v) al)) *clip-cache*)
       (%write-state "clip-cache.lisp" al)))
(defun %clip-cached (source page)
  "キャッシュ済みなら (values 絶対パス ページ名)。無ければ nil。"
  (let ((v (gethash (cons source page) *clip-cache*)))
    (when (consp v)
      (let ((p (concatenate 'string (%clip-cache-dir) (first v))))
        (when (dotnet:static "System.IO.File" "Exists" p)
          (values p (second v)))))))
(defun %clip-cache-drop (item)
  "ITEM = (source . page) のキャッシュファイルとマニフェスト項目を消す (素材削除時)。"
  (let ((v (gethash item *clip-cache*)))
    (when (consp v)
      (ignore-errors
        (let ((p (concatenate 'string (%clip-cache-dir) (first v))))
          (when (dotnet:static "System.IO.File" "Exists" p)
            (dotnet:static "System.IO.File" "Delete" p))))
      (remhash item *clip-cache*)
      (save-clip-cache))))
(defun %clip-cache-clear ()
  "全キャッシュを破棄 (clip-clear 時)。"
  (ignore-errors
    (when (dotnet:static "System.IO.Directory" "Exists" (%clip-cache-dir))
      (dotnet:static "System.IO.Directory" "Delete" (%clip-cache-dir) t)))
  (clrhash *clip-cache*)
  (save-clip-cache))
(defun %clip-cache-warm ()
  "未キャッシュのクリップ項目を背景でローカルコピー。source ごとにまとめて 1 回ずつ open
するので NAS の再 open を最小化する。冪等 (既にあるものはスキップ)。"
  (make-thread
   (lambda ()
     (handler-case
         (let ((by-source (make-hash-table :test 'equal)) (changed nil))
           (dolist (item *clip*)
             (unless (gethash item *clip-cache*)
               (push (cdr item) (gethash (car item) by-source))))
           (maphash
            (lambda (source pages)
              (handler-case
                  (let ((book (open-book source)))
                    (unwind-protect
                         (dolist (page pages)
                           (handler-case
                               (multiple-value-bind (bytes ext) (book-page-bytes book page)
                                 (let* ((name (concatenate 'string
                                                           (%fnv1a-hex (format nil "~a#~d" source page)) "." ext))
                                        (path (concatenate 'string (%clip-cache-dir) name)))
                                   (dotnet:static "System.IO.Directory" "CreateDirectory" (%clip-cache-dir))
                                   (dotnet:static "System.IO.File" "WriteAllBytes" path bytes)
                                   (setf (gethash (cons source page) *clip-cache*)
                                         (list name (book-page-name book page))
                                         changed t)))
                             (error (e) (log-err "clip-cache-page" e))))
                      (ignore-errors (book-close book))))
                (error (e) (log-err "clip-cache-source" e))))
            by-source)
           (when changed (save-clip-cache)))
       (error (e) (log-err "clip-cache-warm" e))))
   :name "paalam-clip-cache"))

(defun %clip-cache-gc ()
  "どのクリップ (*clips*) からも参照されない (source.page) のキャッシュを掃除 (#35 ③)。"
  (let ((live (make-hash-table :test 'equal)))
    (dolist (c *clips*) (dolist (it (cdr c)) (setf (gethash it live) t)))
    (let (dead)
      (maphash (lambda (k v) (declare (ignore v)) (unless (gethash k live) (push k dead))) *clip-cache*)
      (dolist (k dead) (%clip-cache-drop k)))))

;;; 名前付き複数クリップの切替/作成/リネーム/削除 (#35 ③)。素材ペインのバーから操作。
(defun %clip-pane-refresh-if-visible (vw)
  (when (and (viewer-clip-pane vw) (dotnet:invoke (viewer-clip-pane vw) "get_IsVisible"))
    (%rebuild-clip-pane vw)))
(defun %clip-switch (vw index)
  "INDEX のクリップへ切替え (データ + ペイン/タイトルのみ)。**コンボの Items は作り直さない**:
これはコンボの SelectionChanged から呼ばれるので、ここで Clear+Add すると再入してコンボの
レイアウト (高さ) が潰れ「選んだら二度と選べない」状態になる。Items 更新は新規/リネーム/削除側で。"
  (when (and (>= index 0) (< index (length *clips*)) (/= index *current-clip*))
    (%clip-sync)
    (setf *current-clip* index *clip* (cdr (clip-current-cell))
          ;; 新クリップではカーソル/選択/pending をリセット (index は内容依存; #35 ④)
          (viewer-clip-cursor vw) 0 (viewer-clip-selected vw) nil
          (viewer-clip-pending vw) nil (viewer-clip-count vw) nil)
    (save-clip) (%clip-cache-warm) (%update-title vw)
    (%clip-pane-refresh-if-visible vw)))
(defun %clip-new (vw name)
  "NAME の空クリップを足して切替え (Items を作り直す)。"
  (%clip-sync)
  (setf *clips* (append *clips* (list (cons name nil)))
        *current-clip* (1- (length *clips*)) *clip* (cdr (clip-current-cell)))
  (save-clip) (%clip-cache-warm) (%update-title vw)
  (%refresh-clip-selector vw) (%clip-pane-refresh-if-visible vw))
(defun %clip-rename (vw name)
  (setf (car (clip-current-cell)) name)
  (save-clip) (%update-title vw) (%refresh-clip-selector vw))
(defun %clip-delete (vw)
  "カレントクリップを削除 (最後の1個は消さない)。参照されなくなったキャッシュも掃除。"
  (when (> (length *clips*) 1)
    (setf *clips* (loop for c in *clips* for i from 0 unless (= i *current-clip*) collect c)
          *current-clip* (max 0 (min *current-clip* (1- (length *clips*))))
          *clip* (cdr (clip-current-cell)))
    (%clip-cache-gc) (save-clip) (%clip-cache-warm) (%update-title vw)
    (%refresh-clip-selector vw) (%clip-pane-refresh-if-visible vw)))
(defun %refresh-clip-selector (vw)
  "クリップ選択コンボを *clips* で作り直し、カレントを選択 (選択変更ハンドラは差分時のみ切替)。"
  (let ((combo (viewer-clip-combo vw)))
    (when combo
      (let ((items (dotnet:invoke combo "get_Items")))
        (dotnet:invoke items "Clear")
        (dolist (c *clips*) (dotnet:invoke items "Add" (car c)))
        (dotnet:invoke combo "set_SelectedIndex" *current-clip*)))))
(defun %build-clip-bar (vw)
  "素材ペイン上部のクリップ選択バー (コンボ + ＋/名前/削除)。viewer-clip-combo に保持。"
  (flet ((sb (label thunk)
           (let ((b (dotnet:new "Avalonia.Controls.Button")))
             (dotnet:invoke b "set_Content" label)
             (dotnet:invoke b "set_Padding" (dotnet:new "Avalonia.Thickness" 6d0 2d0 6d0 2d0))
             (dotnet:invoke b "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 4d0 0d0))
             (dotnet:add-event b "Click" (lambda (s e) (declare (ignore s e)) (funcall thunk)))
             b)))
    (let ((bar   (dotnet:new "Avalonia.Controls.StackPanel"))
          (combo (dotnet:new "Avalonia.Controls.ComboBox"))
          (btns  (dotnet:new "Avalonia.Controls.StackPanel")))
      (setf (viewer-clip-combo vw) combo)
      (dotnet:invoke combo "set_HorizontalAlignment" (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Stretch"))
      (dotnet:invoke combo "set_MinWidth" 150d0)   ; Stretch が効かない時用の最小幅 (ペイン 220px)
      (dotnet:add-event combo "SelectionChanged"
        (lambda (s e) (declare (ignore s e))
          (let ((i (dotnet:invoke combo "get_SelectedIndex")))
            (when (and (>= i 0) (/= i *current-clip*)) (%clip-switch vw i)))))
      (dotnet:invoke btns "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
      (dotnet:invoke btns "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 4d0 0d0 0d0))
      (let ((c (dotnet:invoke btns "get_Children")))
        (dotnet:invoke c "Add" (sb "＋" (lambda () (%prompt-text (viewer-window vw) "新規クリップ名" ""
                                                                 (lambda (n) (%clip-new vw n))))))
        (dotnet:invoke c "Add" (sb "名前" (lambda () (%prompt-text (viewer-window vw) "クリップ名を変更" (clip-current-name)
                                                                   (lambda (n) (%clip-rename vw n))))))
        (dotnet:invoke c "Add" (sb "削除" (lambda () (%clip-delete vw)))))
      (dotnet:invoke bar "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 0d0 4d0))
      (let ((c (dotnet:invoke bar "get_Children")))
        ;; このペインが「素材クリップ」であることを示す見出し (#35 ③)。
        (dotnet:invoke c "Add" (%s-header "素材クリップ (ページの一時カゴ → 掃き出し)"))
        (dotnet:invoke c "Add" combo) (dotnet:invoke c "Add" btns))
      (%refresh-clip-selector vw)
      bar)))

(defun %sanitize (s)
  (map 'string (lambda (c) (if (or (alphanumericp c) (member c '(#\- #\_ #\.))) c #\_))
       (or s "x")))

(defun %export-resize-bytes (bytes ext)
  "*export-width* が整数なら、幅がそれを超えるページだけ縮小して PNG の bytes を返す (#35 ②)。
原寸以下 / :original は無変換で (values bytes ext)。失敗時も原寸で返す (掃き出しは止めない)。
Avalonia の Bitmap で decode→CreateScaledBitmap→PNG Save (サムネ #6 と同系の経路)。"
  (let ((target *export-width*))
    (if (and (integerp target) (plusp target))
        (handler-case
            (let ((ms (dotnet:new "System.IO.MemoryStream")) (bmp nil) (scaled nil))
              (unwind-protect
                   (progn
                     ;; MemoryStream(byte[]) ctor は overload バグ (#294) を踏むので Write で渡す。
                     (dotnet:invoke ms "Write" bytes 0 (dotnet:invoke bytes "get_Length"))
                     (dotnet:invoke ms "set_Position" 0)
                     (setf bmp (dotnet:new "Avalonia.Media.Imaging.Bitmap" ms))
                     (let* ((px (dotnet:invoke bmp "get_PixelSize"))
                            (w  (dotnet:invoke px "get_Width")))
                       (if (<= w target)
                           (values bytes ext)        ; 原寸以下は無変換で素通し
                           (let* ((h   (dotnet:invoke px "get_Height"))
                                  (th  (max 1 (round (* h target) w)))
                                  (out (dotnet:new "System.IO.MemoryStream")))
                             (setf scaled
                                   (dotnet:invoke bmp "CreateScaledBitmap"
                                     (dotnet:new "Avalonia.PixelSize" target th)
                                     (dotnet:static "Avalonia.Media.Imaging.BitmapInterpolationMode" "HighQuality")))
                             (unwind-protect
                                  (progn (dotnet:invoke scaled "Save" out)
                                         (values (dotnet:invoke out "ToArray") "png"))
                               (dotnet:invoke out "Dispose"))))))
                (ignore-errors (when scaled (dotnet:invoke scaled "Dispose")))
                (ignore-errors (when bmp (dotnet:invoke bmp "Dispose")))
                (dotnet:invoke ms "Dispose")))
          (error (e) (log-err "export-resize" e) (values bytes ext)))
        (values bytes ext))))

(defun %write-clip-zip (out clip)
  "CLIP の各ページを連番リネームして zip OUT に書き出す (各 source は開き直して読む)。
背景スレッドから呼んで UI を固めない。OUT を返す。"
  (when clip
    (ignore-errors (dotnet:load-assembly "System.IO.Compression"))
    (let* ((fs (dotnet:static "System.IO.File" "Create" out))
           (zip nil)
           (books (make-hash-table :test 'equal))
           (seq 0))
      (unwind-protect
           (progn
             (setf zip (dotnet:new "System.IO.Compression.ZipArchive" fs
                                   (dotnet:static "System.IO.Compression.ZipArchiveMode" "Create")))
             (dolist (item clip)
               (incf seq)
               (handler-case
                   (let ((src (car item)) (page (cdr item)))
                     ;; #35 ①: ローカルキャッシュがあれば NAS を再読みせずそれを詰める。
                     ;; 無ければ従来どおり source を開き直して読む (既存 clip.lisp 互換)。
                     (multiple-value-bind (cpath cname) (%clip-cached src page)
                       (multiple-value-bind (bytes ext name)
                           (if cpath
                               (values (dotnet:static "System.IO.File" "ReadAllBytes" cpath)
                                       (%name-ext cpath) cname)
                               (let ((book (or (gethash src books)
                                               (setf (gethash src books) (open-book src)))))
                                 (multiple-value-bind (b e) (book-page-bytes book page)
                                   (values b e (book-page-name book page)))))
                         ;; #35 ②: 設定された最大幅を超えるページは縮小 (PNG 化) して詰める。
                         (multiple-value-bind (wbytes wext) (%export-resize-bytes bytes ext)
                           (let* ((nm (format nil "~4,'0d_~a.~a" seq (%sanitize (pathname-name name)) wext))
                                  (entry (dotnet:invoke zip "CreateEntry" nm))
                                  (es (dotnet:invoke entry "Open")))
                             (unwind-protect
                                  (dotnet:invoke es "Write" wbytes 0 (dotnet:invoke wbytes "get_Length"))
                               (dotnet:invoke es "Dispose")))))))
                 (error (e) (log-err "export-clip-item" e)))))
        (when zip (ignore-errors (dotnet:invoke zip "Dispose")))
        (ignore-errors (dotnet:invoke fs "Dispose"))
        (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (book-close v))) books))
      out)))

;;; 素材ペイン (#18 Phase 2): 集めたページをサムネ一覧で表示。サムネは背景
;;; スレッドで非同期ロードして後から差し込む (NAS でも UI を固めない)。

(defun %clip-remove-at (vw idx)
  "idx 番目の素材を取り除いてペイン更新。ローカルキャッシュ (#35 ①) も掃除。"
  (let ((item (nth idx *clip*)))
    (setf *clip* (loop for e in *clip* for i from 0 unless (= i idx) collect e))
    (when item (%clip-cache-drop item)))
  (save-clip)
  (%update-title vw)
  (%rebuild-clip-pane vw))

(defun %reorder-target (vw y)
  "clip-stack 内の Y 座標から挿入位置 (0..n) を求める (各 cell の中点で判定)。"
  (let* ((children (dotnet:invoke (viewer-clip-stack vw) "get_Children"))
         (n (dotnet:invoke children "get_Count")))
    (dotimes (i n n)
      (let* ((cell (dotnet:invoke children "get_Item" i))
             (b   (dotnet:invoke cell "get_Bounds"))
             (top (dotnet:invoke b "get_Y"))
             (h   (dotnet:invoke b "get_Height")))
        (when (< y (+ top (/ h 2.0d0))) (return-from %reorder-target i))))))

(defun %clip-reorder (vw from to)
  "from の素材を、元リスト基準の挿入位置 to (0..n) へ移してペイン更新。"
  (when (and from (/= from to) (>= from 0) (< from (length *clip*)))
    (let* ((item (nth from *clip*))
           (rest (loop for e in *clip* for i from 0 unless (= i from) collect e))
           (pos  (max 0 (min (if (> to from) (1- to) to) (length rest)))))
      (setf *clip* (append (subseq rest 0 pos) (list item) (subseq rest pos)))
      (save-clip)
      (%rebuild-clip-pane vw))))

;;; 並べ替えの視覚フィードバック (#20): 掴んだサムネを半透明化 + 挿入位置に青い線。
(defun %ensure-reorder-line (vw)
  "挿入位置インジケータ Border を用意 (使い回し)。"
  (or (viewer-reorder-line vw)
      (setf (viewer-reorder-line vw)
            (let ((b (dotnet:new "Avalonia.Controls.Border")))
              (dotnet:invoke b "set_Height" 3d0)
              (dotnet:invoke b "set_Margin" (dotnet:new "Avalonia.Thickness" 4d0 0d0 4d0 0d0))
              (dotnet:invoke b "set_Background" (dotnet:static "Avalonia.Media.Brushes" "DeepSkyBlue"))
              b))))

(defun %show-reorder-line (vw y)
  "clip-stack の Y にドラッグ中、挿入予定位置へインジケータを移動。"
  (let ((children (dotnet:invoke (viewer-clip-stack vw) "get_Children"))
        (line (%ensure-reorder-line vw)))
    ;; いったん外してから対象位置を計算 (line 自身が index をずらさないように)。
    (ignore-errors (dotnet:invoke children "Remove" line))
    (dotnet:invoke children "Insert" (%reorder-target vw y) line)))

(defun %clear-reorder-visual (vw)
  "挿入線を外し、掴んだサムネの不透明度を戻す (並べ替え終了/中断時)。"
  (let ((line (viewer-reorder-line vw)) (img (viewer-reorder-img vw)))
    (when line
      (ignore-errors
        (dotnet:invoke (dotnet:invoke (viewer-clip-stack vw) "get_Children") "Remove" line)))
    (when img (ignore-errors (dotnet:invoke img "set_Opacity" 1d0)))))

;;; クリップ全体を 1 冊の仮想本として閲覧する (#35 ⑤ プレビュー)。クリップの各 (source . page)
;;; をページに持つ book プロトコル実装。読み込みは①のローカルキャッシュ優先 (NAS 再読み回避)、
;;; 無ければ source を開いて読む (source ハンドルは lazy にキャッシュ・ロックで保護)。
(defclass clip-book ()
  ((items   :initarg :items   :accessor cb-items)        ; (source . page) の vector
   (sources :initform (make-hash-table :test 'equal) :accessor cb-sources)  ; source -> 開いた book
   (lock    :initform (make-lock "clip-book") :accessor cb-lock)))
(defun make-clip-book (items) (make-instance 'clip-book :items (coerce items 'vector)))
(defmethod book-page-count ((b clip-book)) (length (cb-items b)))
(defmethod book-page-name ((b clip-book) i)
  (let ((it (aref (cb-items b) i)))
    (format nil "~a:~d" (file-namestring (car it)) (1+ (cdr it)))))
(defmethod book-parallel-safe-p ((b clip-book)) t)   ; 1 ハンドル共有 + 内部ロック
(defun %cb-source (b source)
  "source の book を開いて (なければ) 返す。cb-lock 下で呼ぶこと。"
  (or (gethash source (cb-sources b))
      (setf (gethash source (cb-sources b)) (open-book source))))
(defun %decode-bytes (bytes)
  "byte[] を Avalonia Bitmap にデコード (MemoryStream は #294 回避で Write)。"
  (let ((ms (dotnet:new "System.IO.MemoryStream")))
    (unwind-protect
         (progn (dotnet:invoke ms "Write" bytes 0 (dotnet:invoke bytes "get_Length"))
                (dotnet:invoke ms "set_Position" 0)
                (dotnet:new "Avalonia.Media.Imaging.Bitmap" ms))
      (dotnet:invoke ms "Dispose"))))
(defmethod book-load-image ((b clip-book) i)
  (let* ((it (aref (cb-items b) i))
         (cpath (%clip-cached (car it) (cdr it))))
    (if cpath
        ;; ローカルキャッシュ (①) からデコード: ロック不要で並列に効く。
        (%decode-bytes (dotnet:static "System.IO.File" "ReadAllBytes" cpath))
        ;; フォールバック: source を開いて読む (ハンドル共有なのでロック)。
        (with-lock-held ((cb-lock b))
          (book-load-image (%cb-source b (car it)) (cdr it))))))
(defmethod book-page-bytes ((b clip-book) i)
  (let* ((it (aref (cb-items b) i))
         (cpath (%clip-cached (car it) (cdr it))))
    (if cpath
        (values (dotnet:static "System.IO.File" "ReadAllBytes" cpath) (%name-ext cpath))
        (with-lock-held ((cb-lock b)) (book-page-bytes (%cb-source b (car it)) (cdr it))))))
(defmethod book-close ((b clip-book))
  (with-lock-held ((cb-lock b))
    (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (book-close v))) (cb-sources b))
    (clrhash (cb-sources b))))

(defun %open-clip-as-book (vw)
  "現在のクリップ全体を 1 冊の本として主ビューアで開く (#35 ⑤)。空なら何もしない。"
  (let ((items (copy-list *clip*)))
    (if (null items)
        (%status vw "クリップが空です")
        (handler-case
            (let ((book (make-clip-book items))
                  (old-loader (viewer-loader vw)) (old-book (viewer-book vw)))
              (ignore-errors (loader-quiesce old-loader))
              (setf *page-direction* (%book-direction book)
                    (viewer-source vw) nil          ; パス無し (履歴/再オープン対象外)
                    (viewer-rotation vw) 0
                    (viewer-search-matches vw) nil (viewer-search-index vw) 0)
              (%slideshow-stop vw)
              (setf (viewer-book vw) book
                    (viewer-loader vw) (open-loader (lambda () (make-clip-book items))
                                                    (lambda (i) (%ui-post (lambda () (%on-page-loaded vw i)))))
                    (viewer-spreads vw) (compute-spreads (book-page-count book))
                    (viewer-sindex vw) 0)
              (%update-title vw)
              (show-spread vw)
              (%thumb-pane-refresh vw)
              (%defer-free old-loader old-book)
              (%focus-viewport vw))         ; ボタンにフォーカスを残さない (Space 等を食われる)
          (error (e) (log-err "open-clip-as-book" e))))))

;;; クリップ編集の vi 操作 (#35 ④)。clip-stack に focus している間だけ効く
;;; (キーは Handled=t で窓の %on-key に漏らさない)。Phase 1=カーソル/移動/削除/追従、
;;; Phase 2=dd/yy→レジスタ→p/P (並べ替え・クリップ間移動)・Space 複数選択・count。
(defparameter *clip-register* nil
  "cut/yank のレジスタ ((source . page) ...)。クリップ間で共有 → p で別クリップへ移植可。")

(defun %clip-cursor-highlight (vw)
  "各セルの枠でカーソル(青)/選択(金)/両方(橙)を表示し、カーソルを表示範囲へスクロール。"
  (let* ((children (dotnet:invoke (viewer-clip-stack vw) "get_Children"))
         (n (dotnet:invoke children "get_Count"))
         (cur (viewer-clip-cursor vw))
         (sel (viewer-clip-selected vw)))
    (dotimes (i n)
      (ignore-errors
        (dotnet:invoke (dotnet:invoke children "get_Item" i) "set_BorderBrush"
                       (dotnet:static "Avalonia.Media.Brushes"
                                      (cond ((and (= i cur) (member i sel)) "Orange")
                                            ((= i cur) "DeepSkyBlue")
                                            ((member i sel) "Gold")
                                            (t "Transparent"))))))
    ;; カーソルが画面外へ出ないよう ScrollViewer.Offset を直接動かして追従 (#35 ④)。
    (let ((scr (viewer-clip-scroll vw)))
      (when (and scr (plusp n) (< cur n))
        (ignore-errors
          (let* ((b   (dotnet:invoke (dotnet:invoke children "get_Item" cur) "get_Bounds"))
                 (cy  (dotnet:invoke b "get_Y"))
                 (ch  (dotnet:invoke b "get_Height"))
                 (vh  (dotnet:invoke (dotnet:invoke scr "get_Viewport") "get_Height"))
                 (off (dotnet:invoke scr "get_Offset"))
                 (oy  (dotnet:invoke off "get_Y"))
                 (ny  (cond ((< cy oy) cy)
                            ((> (+ cy ch) (+ oy vh)) (max 0d0 (- (+ cy ch) vh)))
                            (t oy))))
            (when (/= ny oy)
              (dotnet:invoke scr "set_Offset" (dotnet:new "Avalonia.Vector" 0d0 ny)))))))))

(defun %clip-cursor-move (vw d)
  (let ((n (length *clip*)))
    (when (plusp n)
      (setf (viewer-clip-cursor vw) (max 0 (min (1- n) (+ (viewer-clip-cursor vw) d))))
      (%clip-cursor-highlight vw))))

(defun %clip-item= (a b)
  (and (string-equal (car a) (car b)) (eql (cdr a) (cdr b))))
(defun %clip-target-indices (vw count)
  "操作対象 index 群 (昇順)。選択があればそれ、無ければカーソルから count 個 (既定1)。"
  (let ((sel (viewer-clip-selected vw)))
    (if sel
        (sort (remove-duplicates (copy-list sel)) #'<)
        (let ((cur (viewer-clip-cursor vw)) (n (length *clip*)))
          (loop for i from cur below (min n (+ cur (max 1 (or count 1)))) collect i)))))
(defun %clip-cut (vw count)
  "対象を取り除いてレジスタへ (cut)。キャッシュは残す (p で移植する可能性があるため)。"
  (let ((idxs (%clip-target-indices vw count)))
    (when idxs
      (setf *clip-register* (mapcar (lambda (i) (nth i *clip*)) idxs)
            ;; 注: LOOP の `it` は when/unless のテスト値を指す予約語なので変数名に使わない。
            *clip* (loop for x in *clip* for i from 0 unless (member i idxs) collect x)
            (viewer-clip-selected vw) nil
            (viewer-clip-cursor vw) (max 0 (min (first idxs) (max 0 (1- (length *clip*))))))
      (save-clip)
      (%status vw (format nil "~d 項目を切り取り" (length idxs)))
      (%rebuild-clip-pane vw))))
(defun %clip-yank (vw count)
  "対象をレジスタへコピー (yank、消さない)。"
  (let ((idxs (%clip-target-indices vw count)))
    (when idxs
      (setf *clip-register* (mapcar (lambda (i) (nth i *clip*)) idxs)
            (viewer-clip-selected vw) nil)
      (%status vw (format nil "~d 項目をコピー" (length idxs)))
      (%clip-cursor-highlight vw))))
(defun %clip-paste (vw before)
  "レジスタを現在のクリップのカーソル位置へ挿入 (before=t で手前)。既出は skip。
別クリップへ切替えてから p すれば「ページをクリップ間で移動」になる。"
  (when *clip-register*
    (let* ((cur (viewer-clip-cursor vw))
           (pos (min (length *clip*) (if before cur (1+ cur))))
           (fresh (remove-if (lambda (it) (member it *clip* :test #'%clip-item=)) *clip-register*)))
      (when fresh
        (setf *clip* (append (subseq *clip* 0 pos) fresh (subseq *clip* pos))
              (viewer-clip-cursor vw) pos)
        (save-clip)
        (%clip-cache-warm)               ; 移植先 source 分のローカルキャッシュを確実化
        (%status vw (format nil "~d 項目を貼り付け" (length fresh)))
        (%rebuild-clip-pane vw)))))
(defun %clip-toggle-select (vw)
  (let ((cur (viewer-clip-cursor vw)))
    (setf (viewer-clip-selected vw)
          (if (member cur (viewer-clip-selected vw))
              (remove cur (viewer-clip-selected vw))
              (cons cur (viewer-clip-selected vw))))
    (%clip-cursor-highlight vw)))

(defun %rebuild-clip-pane (vw)
  "clip-stack を作り直す。キャッシュ済みサムネは即表示、未キャッシュのみ背景で
ロードして %thumb-cache に貯めつつ差し込む (二度目以降は source 再取得なし)。"
  (let* ((stack (viewer-clip-stack vw))
         (children (dotnet:invoke stack "get_Children"))
         (snapshot (copy-list *clip*))
         (pending '()))                  ; (img . (source . page)) 未キャッシュ分
    (dotnet:invoke children "Clear")
    (loop for item in snapshot for i from 0 do
      (let ((img  (dotnet:new "Avalonia.Controls.Image"))
            (cell (dotnet:new "Avalonia.Controls.Grid"))   ; img + × を重ねる
            (xbtn (dotnet:new "Avalonia.Controls.Button"))
            (hl   (dotnet:new "Avalonia.Controls.Border")) ; カーソル枠 (#35 ④)
            (idx i)
            (cached (%thumb-get (car item) (cdr item))))
        (dotnet:invoke img "set_Width" 200d0)
        (dotnet:invoke img "set_Stretch" (dotnet:static "Avalonia.Media.Stretch" "Uniform"))
        ;; 左ドラッグで並べ替え (#18 Phase 3)。press で掴み、release 位置へ挿入。
        ;; ドラッグ中はサムネを半透明化 + 挿入位置に青線を出す (#20)。
        (dotnet:add-event img "PointerPressed"
          (lambda (s e) (declare (ignore s))
            (dotnet:invoke e "set_Handled" t)   ; ウィンドウのパンを起こさない
            (when (dotnet:invoke (dotnet:invoke (dotnet:invoke e "GetCurrentPoint" img)
                                                "get_Properties") "get_IsLeftButtonPressed")
              (setf (viewer-clip-cursor vw) idx)                ; #35 ④ カーソルをここへ
              (ignore-errors (dotnet:invoke stack "Focus"))     ; 編集コンテキスト (stack focus) へ
              (%clip-cursor-highlight vw)
              (setf (viewer-reorder-from vw) idx (viewer-reorder-img vw) img)
              (dotnet:invoke img "set_Opacity" 0.4d0)   ; 掴んでる感
              (%ensure-reorder-line vw)
              (ignore-errors (dotnet:invoke (dotnet:invoke e "get_Pointer") "Capture" img)))))
        (dotnet:add-event img "PointerMoved"
          (lambda (s e) (declare (ignore s))
            (when (viewer-reorder-from vw)
              (%show-reorder-line vw
                (dotnet:invoke (dotnet:invoke e "GetPosition" (viewer-clip-stack vw)) "get_Y")))))
        (dotnet:add-event img "PointerReleased"
          (lambda (s e) (declare (ignore s))
            (dotnet:invoke e "set_Handled" t)
            (let ((from (viewer-reorder-from vw)))
              (when from
                (let ((y (dotnet:invoke (dotnet:invoke e "GetPosition" (viewer-clip-stack vw)) "get_Y")))
                  (%clear-reorder-visual vw)   ; target 計算前に線を外す (index ずれ防止)
                  (let ((target (%reorder-target vw y)))
                    (setf (viewer-reorder-from vw) nil (viewer-reorder-img vw) nil)
                    (ignore-errors (dotnet:invoke (dotnet:invoke e "get_Pointer") "Capture" nil))
                    (%clip-reorder vw from target)))))))
        ;; × ボタン (右上)。赤背景+白太字で削除と分かるよう目立たせる。
        (dotnet:invoke xbtn "set_Content" "×")
        (dotnet:invoke xbtn "set_Foreground" (dotnet:static "Avalonia.Media.Brushes" "White"))
        (dotnet:invoke xbtn "set_Background" (dotnet:static "Avalonia.Media.Brushes" "Firebrick"))
        (dotnet:invoke xbtn "set_FontWeight" (dotnet:static "Avalonia.Media.FontWeight" "Bold"))
        (dotnet:invoke xbtn "set_FontSize" 14d0)
        (dotnet:invoke xbtn "set_HorizontalAlignment"
                       (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Right"))
        (dotnet:invoke xbtn "set_VerticalAlignment"
                       (dotnet:static "Avalonia.Layout.VerticalAlignment" "Top"))
        (dotnet:invoke xbtn "set_Padding" (dotnet:new "Avalonia.Thickness" 7d0 2d0))
        (dotnet:add-event xbtn "Click"
                          (lambda (s e) (declare (ignore s e)) (%clip-remove-at vw idx)))
        (dotnet:invoke cell "set_Margin" (dotnet:new "Avalonia.Thickness" 4d0))
        (let ((cc (dotnet:invoke cell "get_Children")))
          (dotnet:invoke cc "Add" img)
          (dotnet:invoke cc "Add" xbtn))
        (if cached
            (dotnet:invoke img "set_Source" cached)
            (push (cons img item) pending))
        (dotnet:invoke hl "set_BorderThickness" (dotnet:new "Avalonia.Thickness" 2d0))  ; #35 ④ カーソル枠
        (dotnet:invoke hl "set_Child" cell)
        (dotnet:invoke children "Add" hl)))
    ;; カーソルを範囲内へ収め、枠ハイライトを反映 (#35 ④)。
    (let ((n (length snapshot)))
      (setf (viewer-clip-cursor vw) (max 0 (min (viewer-clip-cursor vw) (max 0 (1- n))))))
    (%clip-cursor-highlight vw)
    (when pending
      (setf pending (nreverse pending))
      (make-thread
       (lambda ()
         (let ((books (make-hash-table :test 'equal)))
           (unwind-protect
                (dolist (pair pending)
                  (ignore-errors
                    (let* ((img (car pair)) (item (cdr pair))
                           (src (car item)) (page (cdr item))
                           (book (or (gethash src books)
                                     (setf (gethash src books) (open-book src))))
                           (bmp (book-load-image book page)))
                      (when bmp
                        (%thumb-put src page bmp)
                        (%ui-post (lambda () (dotnet:invoke img "set_Source" bmp)))))))
             (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (book-close v))) books))))
       :name "paalam-thumbs"))))

(defun %do-export (vw out)
  "out へ素材を背景スレッドで書き出し、完了後に UI を更新 (clip クリア)。"
  (let ((snapshot (copy-list *clip*)))
    (make-thread
     (lambda ()
       (ignore-errors (%write-clip-zip out snapshot))
       (%ui-post (lambda ()
                   (clip-clear) (%update-title vw) (%rebuild-clip-pane vw)
                   (dotnet:invoke (viewer-window vw) "set_Title"
                                  (format nil "掃き出し完了 → ~a" out)))))
     :name "paalam-export")))

(defun %pane-export (vw)
  "保存ダイアログで出力先を選び、背景で zip に掃き出す。"
  (when *clip*
    (let ((suggested (concatenate 'string "paalam-clip-"
                                  (dotnet:invoke (dotnet:static "System.DateTime" "Now")
                                                 "ToString" "yyyyMMdd-HHmmss")
                                  ".zip")))
      (dotnet:static "Paalam.DialogHelper" "PickSaveZip" (viewer-window vw) suggested
                     (dotnet:make-delegate "System.Action`1[System.String]"
                                           (lambda (out) (%do-export vw out)))))))

(defun %toggle-clip-pane (vw)
  ;; #35 ④: P = トグル。開く時はそのまま編集できるようクリップへ focus、閉じる時はビューへ。
  ;; クリップ編集中に view を覗くだけなら Esc (閉じずに view へ)。閉じる専用キーの洗練と
  ;; キーのカスタマイズは follow-up (今は p=ペーストが P を食うので focus 判定の閉じは置かない)。
  (let* ((pane (viewer-clip-pane vw))
         (vis  (dotnet:invoke pane "get_IsVisible")))
    (dotnet:invoke pane "set_IsVisible" (not vis))
    (cond ((not vis)                       ; これから表示 → 再構築してクリップへ focus
           (%rebuild-clip-pane vw)
           (%defer (lambda ()
                     (ignore-errors (dotnet:invoke (viewer-clip-stack vw) "Focus"))
                     (%clip-cursor-highlight vw))
                   120))
          (t (%focus-viewport vw)))))      ; これから非表示 → ビューへ戻す

(defun %clip-drop (vw path)
  "ペインにドロップされた path を素材に追加 (画像=親フォルダ+そのページ、
フォルダ/書庫=全ページ)。"
  (handler-case
      (if (and (dotnet:static "System.IO.File" "Exists" path)
               (image-ext-p (pathname-type path)))
          ;; 画像: 親フォルダ (drive 込み) を source、その中での index を page。
          ;; directory-namestring は drive を落とすので Path.GetDirectoryName を使う。
          (let* ((dir (substitute #\/ #\\
                                  (dotnet:static "System.IO.Path" "GetDirectoryName" path)))
                 (files (%dir-image-files dir))
                 (idx (or (position (file-namestring path) files
                                    :key #'file-namestring :test #'string-equal)
                          0)))
            (clip-add dir idx))
          ;; フォルダ/書庫: 全ページ。
          (multiple-value-bind (book start) (open-book path)
            (declare (ignore start))
            (unwind-protect
                 (dotimes (p (book-page-count book)) (clip-add path p))
              (ignore-errors (book-close book)))))
    (error (e) (log-err "clip-drop" e)))
  (%clip-cache-warm)                   ; #35 ①: 追加分を背景でローカルコピー
  (%update-title vw)
  (when (dotnet:invoke (viewer-clip-pane vw) "get_IsVisible") (%rebuild-clip-pane vw)))

