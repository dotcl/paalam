;;;; settings.lisp — paalam (#39 main.lisp 分割): 表示設定・永続化・履歴・ブックマーク
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; 表示設定・永続化・履歴・ブックマーク (viewer 本体は viewer.lisp)
;;; ------------------------------------------------------------------

;;; --- 表示設定 (将来のカスタマイズ UI / 設定ファイルの対象) ---------
(defparameter *page-direction* :rtl
  "現在の本の綴じ方向 (実行時の値)。:rtl=右開き/漫画 (← が次・→ が前)、:ltr=左開き。
本を開くと種類で *image-direction* / *pdf-direction* から決まり、D キーでその場反転 (#33)。")
(defparameter *image-direction* :rtl
  "画像/書庫 (漫画) を開いた時の既定綴じ方向 (#33)。")
(defparameter *pdf-direction* :ltr
  "PDF (論文/技術書) を開いた時の既定綴じ方向 (#33)。")
(defun %book-direction (book)
  "本の種類ごとの既定綴じ方向 (#33)。pdf=*pdf-direction* / 画像・書庫=*image-direction*。"
  (if (typep book 'pdf-book) *pdf-direction* *image-direction*))
(defparameter *spread* t
  "見開き表示。t = 2 ページ並べ、nil = 単ページ。")
(defparameter *first-page-single* t
  "見開き時、先頭ページ (表紙) を単独表示する (漫画の慣習)。")
(defparameter *recursive* t
  "フォルダを本として開くとき、サブフォルダの画像も再帰的に集める。")
(defparameter *fit-mode* :whole
  "フィットモード。:whole=全体(中央), :width=幅フィット(縦スクロール),
:height=高フィット(横スクロール)。実効スケール = フィット基準 × user zoom (#15)。")
(defparameter *background-color* "Black"
  "ウィンドウ背景 (= フィット時の余白/レターボックス) のブラシ名。Avalonia.Media.Brushes の名前 (#2)。")
(defparameter *interpolation* :high
  "拡大時の補間品質。:high=HighQuality (なめらか), :low=LowQuality (くっきり寄り) (#2)。")
(defparameter *theme* :system
  "UI テーマ (メニュー/ダイアログ等 FluentTheme 部の配色)。:system=OS のライト/ダークに
追従 (ThemeVariant.Default)、:light / :dark で固定 (#27)。画像表示の背景は別途 *background-color*。")

;;; --- 設定の永続化 (s式 plist / OS の app data) ---------------------
(defparameter *export-width* :original
  "掃き出し時の最大幅 (#35 ②)。:original=原寸 (変換なし) / 整数=その幅を超えるページだけ
縮小して PNG 出力。原寸以下のページは無変換で素通し (上げない・再エンコードしない)。")

(defparameter *settings*
  '((:image-direction   . *image-direction*)
    (:pdf-direction     . *pdf-direction*)
    (:export-width      . *export-width*)
    (:spread            . *spread*)
    (:first-page-single . *first-page-single*)
    (:recursive         . *recursive*)
    (:fit-mode          . *fit-mode*)
    (:background-color  . *background-color*)
    (:interpolation     . *interpolation*)
    (:theme             . *theme*)
    (:pdf-dpi           . *pdf-dpi*)
    (:cache-memory-mb   . *cache-memory-mb*)
    (:cache-size        . *cache-size*)
    (:prefetch-ahead    . *prefetch-ahead*)
    (:slideshow-interval . *slideshow-interval*)
    (:loupe-scale       . *loupe-scale*)
    (:loader-threads    . *loader-threads*))
  "保存対象の (設定キー . 特殊変数シンボル)。ここに足すだけで永続化対象になる。")

(defun %config-dir ()
  "per-user 設定ディレクトリ + paalam/。
Win=%APPDATA%, Linux=~/.config (XDG) は GetFolderPath(ApplicationData) が正しく返す。
macOS だけ GetFolderPath が ~/.config を返す .NET の癖があるので
Mac ネイティブの ~/Library/Application Support に寄せる (dotnet/runtime #40353)。"
  (let ((base
          (if (dotnet:static "System.OperatingSystem" "IsMacOS")
              (concatenate 'string
                           (dotnet:static "System.Environment" "GetFolderPath"
                                          (dotnet:static "System.Environment+SpecialFolder" "UserProfile"))
                           "/Library/Application Support")
              (dotnet:static "System.Environment" "GetFolderPath"
                             (dotnet:static "System.Environment+SpecialFolder" "ApplicationData")))))
    (concatenate 'string base "/paalam/")))

(defun %config-file () (concatenate 'string (%config-dir) "settings.lisp"))

(defun save-settings ()
  "現在の設定値を s式 plist で書き出す。"
  (handler-case
      (progn
        (dotnet:static "System.IO.Directory" "CreateDirectory" (%config-dir))
        (with-open-file (s (%config-file) :direction :output
                           :if-exists :supersede :if-does-not-exist :create)
          (let ((plist '()))
            (dolist (e *settings*)
              (setf plist (list* (car e) (symbol-value (cdr e)) plist)))
            (with-standard-io-syntax (prin1 plist s) (terpri s)))))
    (error (e) (log-err "save-settings" e))))

(defun load-settings ()
  "設定ファイルがあれば読み込んで反映。無ければ/壊れていれば既定値で作り直す。"
  (let ((file (%config-file)))
    (handler-case
        (if (dotnet:static "System.IO.File" "Exists" file)
            (with-open-file (s file :direction :input)
              (let* ((*read-eval* nil) (plist (read s nil nil)))
                (when (consp plist)
                  (loop for (k v) on plist by #'cddr
                        for e = (assoc k *settings*)
                        when e do (setf (symbol-value (cdr e)) v)))))
            (save-settings))
      (error (e)
        (log-err "load-settings" e)
        (ignore-errors (save-settings))))))   ; 壊れていたら既定で作り直す

;;; --- 履歴 + ブックマーク (#7) -------------------------------------
;;; 履歴: 本ごとに最後に見たページを覚え、再度開くとそこから再開。
;;; ブックマーク: ページにしおり (B 切替, ★ 表示, [ ] で前後のしおりへ)。
;;; どちらも s式で app data に永続化。横断ブラウズ UI は #6/#9。

(defun %state-file (name) (concatenate 'string (%config-dir) name))

(defun %read-sexp-file (file)
  "FILE があれば 1 個の s式を読んで返す (*read-eval* nil)。無ければ/壊れていれば nil。
任意パス対応 (設定の import や状態ファイル読みの共通土台; #24/#34)。"
  (handler-case
      (when (dotnet:static "System.IO.File" "Exists" file)
        (with-open-file (s file :direction :input)
          (let ((*read-eval* nil)) (read s nil nil))))
    (error (e) (log-err (format nil "read ~a" file) e) nil)))

(defun %atomic-write (file data)
  ;; アトミック書き込み (#34): temp に書いて rename で置換する。複数インスタンス
  ;; 同時起動でも、読み手が書き込み途中の中途半端なファイルを見ない (= partial read
  ;; による parse 失敗→既定で上書き、を防ぐ)。temp 名に PID を入れて他インスタンスの
  ;; temp と衝突させない。同一ディレクトリの File.Move(overwrite) は実質 atomic な rename。
  ;; 任意パス対応 (設定の export と状態ファイル書きの共通土台; #24/#34)。
  (handler-case
      (let* ((dir (dotnet:static "System.IO.Path" "GetDirectoryName" file))
             (tmp (concatenate 'string file "."
                               (princ-to-string (dotnet:static "System.Environment" "ProcessId"))
                               ".tmp")))
        (when (and dir (plusp (length dir)))
          (dotnet:static "System.IO.Directory" "CreateDirectory" dir))
        (unwind-protect
             (progn
               (with-open-file (s tmp :direction :output
                                  :if-exists :supersede :if-does-not-exist :create)
                 (with-standard-io-syntax (prin1 data s) (terpri s)))
               (dotnet:static "System.IO.File" "Move" tmp file t))   ; overwrite=t
          (when (dotnet:static "System.IO.File" "Exists" tmp)
            (ignore-errors (dotnet:static "System.IO.File" "Delete" tmp)))))
    (error (e) (log-err (format nil "write ~a" file) e))))

(defun %read-state (name) (%read-sexp-file (%state-file name)))
(defun %write-state (name data) (%atomic-write (%state-file name) data))

;;; 履歴
(defparameter *history* nil "((path . page) ...) 最近順。")
(defparameter *history-limit* 200)
(defun load-history () (setf *history* (%read-state "history.lisp")))
(defun %merge-history (mine disk)
  "自分の履歴を優先しつつ disk にしか無い path を残してマージ (#34)。複数インスタンス
が別々の本を開閉しても、片方の履歴を last-writer-wins で消さないため。path は
string-equal 相当 (大小無視) で重複判定。新しい順を保ち *history-limit* で切る。"
  (let ((seen (make-hash-table :test 'equal)) (out '()))
    (dolist (e (append mine disk))
      (let ((k (string-downcase (car e))))
        (unless (gethash k seen)
          (setf (gethash k seen) t)
          (push e out))))
    (setf out (nreverse out))
    (if (> (length out) *history-limit*) (subseq out 0 *history-limit*) out)))
(defun save-history ()
  ;; 書く直前にディスクの現状とマージ (他インスタンスの履歴を保全; #34)。
  (setf *history* (%merge-history *history* (%read-state "history.lisp")))
  (%write-state "history.lisp" *history*))
(defun history-page (path) (cdr (assoc path *history* :test #'string-equal)))
(defun history-update (path page)
  (when path
    (setf *history* (cons (cons path page)
                          (remove path *history* :key #'car :test #'string-equal)))
    (when (> (length *history*) *history-limit*)
      (setf *history* (subseq *history* 0 *history-limit*)))))

;;; ブックマーク
(defparameter *bookmarks* nil "((path . page) ...)。")
(defun load-bookmarks () (setf *bookmarks* (%read-state "bookmarks.lisp")))
(defun save-bookmarks () (%write-state "bookmarks.lisp" *bookmarks*))
(defun bookmarked-p (path page)
  (and path (member-if (lambda (e) (and (string-equal (car e) path) (eql (cdr e) page)))
                       *bookmarks*)))
(defun toggle-bookmark (path page)
  (when path
    (if (bookmarked-p path page)
        (setf *bookmarks* (remove-if (lambda (e) (and (string-equal (car e) path)
                                                      (eql (cdr e) page)))
                                     *bookmarks*))
        (push (cons path page) *bookmarks*))
    (save-bookmarks)))
(defun book-bookmark-pages (path)
  (sort (loop for e in *bookmarks* when (string-equal (car e) path) collect (cdr e)) #'<))

