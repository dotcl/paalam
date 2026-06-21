;;;; loader.lisp — paalam (#39 main.lisp 分割): 先読みローダー (#17)
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; 先読みローダー (#17): 単一ワーカースレッドが書庫アクセスを一手に担い、
;;; デコード済み Bitmap を cache。UI は cache を読むだけ。ロード完了は
;;; Dispatcher.UIThread.Post で UI へ通知して差し替え。NAS 上の書庫でも
;;; めくる前に裏で先読みするので快適。
;;; ------------------------------------------------------------------

(defparameter *cache-memory-mb* 256
  "デコード済みページ cache のメモリ上限 (MB)。これが主予算 (#25)。ページの実バイト
 (PixelSize×4) で見積もり、超えたら現在ページから読み方向の後方→遠方の順に Dispose。")
(defparameter *cache-size* 48
  "cache の最大保持数 (副次的な上限; 主はメモリ予算 *cache-memory-mb*)。小さい画像が
 大量に溜まるのを防ぐガード。")
(defparameter *prefetch-ahead* 8
  "現在の見開きから何見開き先まで先読みするか。後方は min(2, これ) 見開きを少しだけ
 先読みする (双方向・前方優先; #25)。設定ファイルで調整可。")
(defparameter *slideshow-interval* 4
  "スライドショーの自動送り間隔 (秒; #28)。")
(defparameter *loupe-scale* 2.5d0
  "ルーペ (拡大鏡) の倍率 (#28)。")
(defparameter *loader-threads* 4
  "並列ローダースレッド数。各スレッドが自前の書庫ハンドルで同時に読むので、
NAS の帯域/レイテンシを隠せる (設定ファイルで調整可)。")

;; 出荷時の設定値スナップショット (全設定変数を定義し終えた今・load-settings の前に取る)。
;; 設定ウィンドウのページ毎「既定に戻す」用 (#27)。*settings* のキー → 既定値。
(defparameter *setting-defaults*
  (mapcar (lambda (e) (cons (car e) (symbol-value (cdr e)))) *settings*)
  "出荷時の (設定キー . 既定値)。各ページ「既定に戻す」が参照する。")

(defstruct loader
  books                               ; 書庫ハンドルの vector (ワーカーが分担)
  (cache (make-hash-table))           ; index -> Avalonia Bitmap
  (inflight (make-hash-table))        ; 現在ロード中の index (二度読み防止)
  (lock (make-lock "paalam-loader"))
  (cv (make-condition-variable))
  (queue '())                         ; ロード待ち index (優先順)
  (current 0)                         ; LRU の中心 (現在ページ index)
  (direction 1)                       ; 読み方向 (+1=index 増 / -1=減; エビクションのバイアス用; #25)
  (done nil)
  workers                             ; ワーカースレッドのリスト
  on-loaded)                          ; (lambda (index)) — UI へ通知

(defun loader-get (ld index)
  (with-lock-held ((loader-lock ld)) (gethash index (loader-cache ld))))

(defun %bmp-bytes (bmp)
  "Bitmap のおおよそのデコード済みバイト数 (PixelSize×4ch)。取れなければ 0。"
  (or (ignore-errors
        (let ((ps (dotnet:invoke bmp "get_PixelSize")))
          (* (dotnet:invoke ps "get_Width") (dotnet:invoke ps "get_Height") 4)))
      0))

(defun %evict-score (k cur dir)
  "エビクション優先度。大きいほど先に捨てる。読み方向(dir)の後方を最優先で捨て、
同方向内は遠い順。現在ページ(s=0)は最小=最後まで残す (#25)。"
  (let ((s (* (- k cur) (if (minusp dir) -1 1))))   ; s>=0: 読み方向の先 / s<0: 後方
    (if (>= s 0) s (+ 1000000 (- s)))))              ; 後方は任意の前方より大

(defun %loader-evict (ld)
  "メモリ予算 (*cache-memory-mb*) と枚数上限 (*cache-size*) を超えたら、現在ページから
読み方向の後方→遠方の順に Dispose する (方向考慮; #25)。Avalonia Bitmap は Skia の
非マネージドメモリを抱えるので必ず Dispose (しないと数十〜百ページで息切れ)。
現在見開きの Bitmap は score 最小なので最後まで残る。"
  (let* ((cache (loader-cache ld)) (cur (loader-current ld)) (dir (loader-direction ld))
         (budget (* *cache-memory-mb* 1024 1024))
         (total 0))
    (maphash (lambda (k v) (declare (ignore k)) (incf total (%bmp-bytes v))) cache)
    (loop while (and (> (hash-table-count cache) 1)        ; 最低 1 枚は残す
                     (or (> total budget) (> (hash-table-count cache) *cache-size*)))
          do (let ((victim nil) (vscore -1))
               (maphash (lambda (k v) (declare (ignore v))
                          (let ((sc (%evict-score k cur dir)))
                            (when (> sc vscore) (setf vscore sc victim k))))
                        cache)
               (when (null victim) (return))
               (let ((bmp (gethash victim cache)))
                 (decf total (%bmp-bytes bmp))
                 (remhash victim cache)
                 (ignore-errors (dotnet:invoke bmp "Dispose")))))))

(defun loader-request (ld indices current)
  "indices を優先順でロード要求 (既キャッシュはスキップ)。current は LRU 中心。"
  (with-lock-held ((loader-lock ld))
    ;; current の変化から読み方向を推定 (エビクションのバイアス用; 0 なら前回方向を維持)。
    (let ((prev (loader-current ld)))
      (setf (loader-direction ld) (cond ((> current prev) 1) ((< current prev) -1)
                                        (t (loader-direction ld)))))
    (setf (loader-current ld) current
          ;; 既にキャッシュ済み or ロード中のものは要求しない (二度読み防止)。
          ;; queue は毎回置き換え = 高速ページ送り時に陳腐な先読み要求は自然に捨てられる (#25 dedup)。
          (loader-queue ld) (remove-if (lambda (i) (or (gethash i (loader-cache ld))
                                                       (gethash i (loader-inflight ld))))
                                       indices))
    (%dbg "req cur=~a want=~a queued=~a cache=~a" current (length indices)
          (length (loader-queue ld)) (hash-table-count (loader-cache ld)))
    (condition-broadcast (loader-cv ld))))   ; 複数ワーカーを起こす

;;; book が並行ロード安全か。directory はファイル read が独立で安全 (1 ハンドル共有可)。
;;; zip/sharp/pdf は書庫/native ハンドルが非並行なので各ワーカーに別ハンドルを持たせる。
(defgeneric book-parallel-safe-p (book))
(defmethod book-parallel-safe-p ((b t)) nil)
(defmethod book-parallel-safe-p ((b directory-book)) t)

(defun %loader-loop (ld book)
  "ワーカー本体。BOOK はこのワーカー専用の書庫ハンドル (並列読み用)。"
  (loop
    (let ((index nil))
      (with-lock-held ((loader-lock ld))
        (loop while (and (null (loader-queue ld)) (not (loader-done ld)))
              do (condition-wait (loader-cv ld) (loader-lock ld)))
        ;; done なら待ち行列 (prefetch) を捨てて即終了。
        (when (loader-done ld) (return))
        (setf index (pop (loader-queue ld)))
        (when index (setf (gethash index (loader-inflight ld)) t)))   ; ロード中マーク
      (when index
        ;; ワーカーは決して死なせない。in-flight は必ず解除。
        (unwind-protect
            (handler-case
                (let ((bmp (book-load-image book index)))   ; 自前ハンドルで読む (並列)
                  (if bmp
                      (progn
                        (with-lock-held ((loader-lock ld))
                          (setf (gethash index (loader-cache ld)) bmp)
                          (%loader-evict ld))
                        (%dbg "cached ~a (cache=~a)" index (hash-table-count (loader-cache ld)))
                        (ignore-errors (funcall (loader-on-loaded ld) index)))
                      (%dbg "LOAD-NIL ~a" index)))
              (error (e) (%dbg "LOAD-ERR ~a : ~a" index e)))
          (with-lock-held ((loader-lock ld)) (remhash index (loader-inflight ld))))))))

(defun open-loader (book-factory on-loaded)
  "BOOK-FACTORY (0 引数で新しい book ハンドルを返す) を *loader-threads* スレッドで並列ロード。
並列安全な book は 1 ハンドル共有、そうでなければスレッド毎に別ハンドルを開く (#35 ⑤ で
clip-book も乗せるため path ではなく factory を取る)。"
  (let* ((n (max 1 *loader-threads*))
         (first (funcall book-factory))
         (nh (if (book-parallel-safe-p first) 1 n))   ; ハンドル数
         (books (coerce (cons first (loop repeat (1- nh) collect (funcall book-factory))) 'vector))
         (ld (make-loader :books books :on-loaded on-loaded)))
    (setf (loader-workers ld)
          (loop for i from 0 below n
                collect (let ((bk (aref books (mod i nh))))
                          (make-thread (lambda () (%loader-loop ld bk)) :name "paalam-loader"))))
    ld))

(defun loader-quiesce (ld)
  "ワーカーを止めて join する (cache の Bitmap/書庫ハンドルは解放しない)。"
  (with-lock-held ((loader-lock ld))
    (setf (loader-done ld) t)
    (condition-broadcast (loader-cv ld)))
  (dolist (w (loader-workers ld)) (ignore-errors (thread-join w))))   ; 全ワーカー終了待ち

(defun loader-free (ld)
  "cache の Bitmap と書庫ハンドルを解放 (quiesce 済み前提)。表示中 Bitmap を破棄するので、
本切替時は描画スレッド(コンポジタ)が触り終えた後 = 次の描画後 (%ui-post) に呼ぶこと。"
  (with-lock-held ((loader-lock ld))
    (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (dotnet:invoke v "Dispose")))
             (loader-cache ld))
    (clrhash (loader-cache ld)))
  (loop for b across (loader-books ld) do (ignore-errors (book-close b))))   ; ハンドル解放

(defun loader-stop (ld)
  "全ワーカーを止め、cache の Bitmap と書庫ハンドルを解放する (即時; 終了時用)。"
  (loader-quiesce ld)
  (loader-free ld))

(defun %defer (thunk &optional (ms 250))
  "THUNK を MS 後に UI スレッドで一度だけ実行。表示中だった Bitmap をコンポジタ(別スレッドの
描画)が触り終える猶予を取って破棄するため (%ui-post の次ティックでは描画完了を保証できない)。"
  (let ((timer (dotnet:new "Avalonia.Threading.DispatcherTimer")))
    (dotnet:invoke timer "set_Interval"
                   (dotnet:static "System.TimeSpan" "FromMilliseconds" (coerce ms 'double-float)))
    (dotnet:add-event timer "Tick"
      (lambda (s e) (declare (ignore s e))
        (dotnet:invoke timer "Stop")
        (ignore-errors (funcall thunk))))
    (dotnet:invoke timer "Start")))

(defun %defer-free (loader book)
  "旧 loader の Bitmap/ハンドルと旧 book の解放を描画後へ遅延し use-after-dispose を回避。"
  (%defer (lambda ()
            (ignore-errors (loader-free loader))
            (ignore-errors (book-close book)))))

(defun %ui-post (thunk)
  "thunk を UI スレッドで実行 (ワーカー→UI マーシャリング)。"
  (let ((act (dotnet:make-delegate "System.Action" thunk)))
    (dotnet:invoke (dotnet:static "Avalonia.Threading.Dispatcher" "UIThread")
                   "Post" act
                   (dotnet:static "Avalonia.Threading.DispatcherPriority" "Normal"))))

;;; viewer: book + 見開き単位 (spreads) + 現在位置 + 2 枚の Image
;;; spreads = 各見開きの page-index リスト (1 or 2 個) の vector。
;;; 単ページ/見開きを同じ仕組みで扱う (単ページ時は全要素 1 個)。
;;; zoom/tx/ty + scale-xf/trans-xf は stage の RenderTransform (ズーム/パン)。
;;; loader = 先読みローダー。
