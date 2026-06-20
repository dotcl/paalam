;;;; main.lisp — paalam 画像ビューア本体 (Avalonia バックエンド; 定義のみ)
;;;;
;;;; GUI は Avalonia (OSS, MIT, クロスプラット)。dotcl host (paalam.exe) が
;;;; DotclHost.Call("RUN-PAALAM" target) を呼び、Lisp 側で AppBuilder を起動する。
;;;; net10.0 (-windows 不要)・STA 不要・Windows Desktop ランタイム非依存。
;;;;
;;;; 「本 (book)」= ページの供給源 (ディレクトリ / アーカイブ) を抽象化した
;;;; CLOS プロトコル。directory-book / zip-book が実装する。GUI 非依存。

(defpackage :paalam
  (:use :cl)
  (:export :run-paalam :build-main-window :open-book
           :book-page-count :book-page-name :book-load-image :book-close))
(in-package :paalam)

;;; define-class (UI クラス emit) とスレッド/ロック/条件変数 (先読みローダー) の contrib。
;;; DotCL.Runtime 0.1.11 は contrib を出力に同梱するので require で解決できる。
;;; *package* = :paalam で require することで、define-class マクロが束縛する `self' が
;;; この下の define-class 本体 (:paalam) と同一シンボルになる。
(require :dotnet-class)
(require :dotcl-thread)
(use-package :dotcl-thread)

(defvar *debug* nil "PAALAM_DEBUG で有効化。先読みの詳細を debug.log に出す。")
(defun %dbg (fmt &rest args)
  (when *debug*
    (ignore-errors
      (with-open-file (s (concatenate 'string (%config-dir) "debug.log")
                         :direction :output :if-exists :append :if-does-not-exist :create)
        (format s "~a~%" (apply #'format nil fmt args))))))

(defun log-err (where e)
  ;; 署名済み .app バンドル内 (BaseDirectory) は書けないので、ホーム配下に出す。
  ;; Finder 起動時の初期化例外 (Avalonia 等) を確実に残すため。
  (ignore-errors
    (with-open-file (s (concatenate 'string
                                    (dotnet:static "System.Environment" "GetFolderPath"
                                                   (dotnet:static "System.Environment+SpecialFolder" "UserProfile"))
                                    "/paalam-error.log")
                       :direction :output :if-exists :append :if-does-not-exist :create)
      (format s "[~a] ~a~%" where e))))

;;; ------------------------------------------------------------------
;;; ファイル名ユーティリティ
;;; ------------------------------------------------------------------

(defparameter *image-extensions* '("jpg" "jpeg" "png" "gif" "bmp" "tif" "tiff" "webp")
  "対応画像拡張子 (小文字)。Avalonia/Skia は webp も読める。")

(defun image-ext-p (ext)
  (and ext (member (string-downcase ext) *image-extensions* :test #'string=) t))

(defun image-name-p (name)
  (let ((dot (position #\. name :from-end t)))
    (and dot (image-ext-p (subseq name (1+ dot))))))

(defun natural-key (s)
  "自然順ソート用キー。数字列をゼロ詰めし英字は小文字化 (page2 < page10)。"
  (with-output-to-string (out)
    (let ((i 0) (n (length s)))
      (loop while (< i n) do
        (let ((c (char s i)))
          (if (digit-char-p c)
              (let ((j i))
                (loop while (and (< j n) (digit-char-p (char s j))) do (incf j))
                (format out "~20,'0d" (parse-integer (subseq s i j)))
                (setf i j))
              (progn (write-char (char-downcase c) out) (incf i))))))))

(defun natural< (a b) (string< (natural-key a) (natural-key b)))

;;; ------------------------------------------------------------------
;;; 画像ロード (Avalonia.Media.Imaging.Bitmap)
;;; ------------------------------------------------------------------

(defun %load-file-bitmap (path)
  (dotnet:new "Avalonia.Media.Imaging.Bitmap" path))   ; Bitmap(string fileName)

(defun %load-zip-bitmap (entry)
  ;; ZipArchiveEntry.Open() は非 seekable なので MemoryStream に写してから渡す。
  ;; Avalonia Bitmap(Stream) は ctor 内で復号するので、戻り後に stream を閉じてよい。
  (let ((es (dotnet:invoke entry "Open"))
        (ms (dotnet:new "System.IO.MemoryStream")))
    (unwind-protect
         (progn (dotnet:invoke es "CopyTo" ms)
                (dotnet:invoke ms "set_Position" 0)
                ;; #294 修正済 (DotCL.Runtime 0.1.12): dotnet:new が Bitmap(Stream) を
                ;; 正しく選べるので C# シム不要 (直呼び)。
                (dotnet:new "Avalonia.Media.Imaging.Bitmap" ms))
      (dotnet:invoke es "Dispose")
      (dotnet:invoke ms "Dispose"))))

;;; ------------------------------------------------------------------
;;; book プロトコル
;;; ------------------------------------------------------------------

(defgeneric book-page-count (book)        (:documentation "ページ数。"))
(defgeneric book-page-name  (book index)  (:documentation "index ページの表示名。"))
(defgeneric book-load-image (book index)  (:documentation "index ページの Avalonia Bitmap。"))
(defgeneric book-close      (book)        (:documentation "リソース解放。"))

;;; directory-book ---------------------------------------------------

(defclass directory-book ()
  ((paths :initarg :paths :reader db-paths)))

(defun %ensure-trailing-slash (dir)
  (if (and (plusp (length dir))
           (member (char dir (1- (length dir))) '(#\/ #\\)))
      dir
      (concatenate 'string dir "/")))

(defun %dedup-strings (list)
  (let ((seen (make-hash-table :test 'equal)) (acc '()))
    (dolist (x list (nreverse acc))
      (unless (gethash x seen) (setf (gethash x seen) t) (push x acc)))))

(defun %dir-image-files (dir)
  "dir 直下 (+ *recursive* ならサブフォルダも) の画像ファイル full path を自然順で。
** は直下を含まないので *.ext と **/*.ext の和集合を取る。"
  (let ((dir (%ensure-trailing-slash dir))
        (globs (if *recursive* '("*." "**/*.") '("*.")))
        (out '()))
    (dolist (ext *image-extensions*)
      (dolist (g globs)
        (dolist (p (directory (concatenate 'string dir g ext)))
          (push (namestring p) out))))
    (sort (%dedup-strings out) #'natural<)))

(defun make-directory-book (dir)
  (make-instance 'directory-book :paths (coerce (%dir-image-files dir) 'vector)))

(defmethod book-page-count ((b directory-book)) (length (db-paths b)))
(defmethod book-page-name  ((b directory-book) i) (file-namestring (aref (db-paths b) i)))
(defmethod book-load-image ((b directory-book) i) (%load-file-bitmap (aref (db-paths b) i)))
(defmethod book-close      ((b directory-book)) nil)

;;; zip-book ---------------------------------------------------------

(defclass zip-book ()
  ((stream  :initarg :stream  :reader zb-stream)
   (archive :initarg :archive :reader zb-archive)
   (names   :initarg :names   :reader zb-names)
   (entries :initarg :entries :reader zb-entries)))

(defun %zip-image-entries (zip)
  (let* ((entries (dotnet:invoke zip "get_Entries"))
         (n (dotnet:invoke entries "get_Count"))
         (acc '()))
    (dotimes (i n)
      (let* ((e (dotnet:invoke entries "get_Item" i))
             (name (dotnet:invoke e "get_FullName")))
        (when (image-name-p name) (push (cons name e) acc))))
    (sort (nreverse acc) #'natural< :key #'car)))

(defun make-zip-book (path)
  (ignore-errors (dotnet:load-assembly "System.IO.Compression"))
  (let* ((fs  (dotnet:static "System.IO.File" "OpenRead" path))
         (zip (dotnet:new "System.IO.Compression.ZipArchive" fs))
         (pairs (%zip-image-entries zip)))
    (make-instance 'zip-book
                   :stream fs :archive zip
                   :names   (coerce (mapcar #'car pairs) 'vector)
                   :entries (coerce (mapcar #'cdr pairs) 'vector))))

(defmethod book-page-count ((b zip-book)) (length (zb-entries b)))
(defmethod book-page-name  ((b zip-book) i) (file-namestring (aref (zb-names b) i)))
(defmethod book-load-image ((b zip-book) i) (%load-zip-bitmap (aref (zb-entries b) i)))
(defmethod book-close ((b zip-book))
  (ignore-errors (dotnet:invoke (zb-archive b) "Dispose"))
  (ignore-errors (dotnet:invoke (zb-stream b) "Dispose")))

;;; sharp-book (SharpCompress: rar/7z/tar 等) -----------------------
;;; zip 以外の書庫を SharpCompress の IArchive 経由で読む。rar/7z/tar など
;;; フォーマット検出は ArchiveFactory.Open が行うので backend は 1 つで済む。

(defun %enum->list (enumerable)
  "IEnumerable を Lisp リストへ (GetEnumerator/MoveNext/Current)。"
  (let ((en (dotnet:invoke enumerable "GetEnumerator")) (acc '()))
    (loop while (dotnet:invoke en "MoveNext")
          do (push (dotnet:invoke en "get_Current") acc))
    (nreverse acc)))

(defun %basename (key)
  "書庫エントリ名 (/ or \\ 区切り) の最終成分。"
  (let ((p (max (or (position #\/ key :from-end t) -1)
                (or (position #\\ key :from-end t) -1))))
    (if (>= p 0) (subseq key (1+ p)) key)))

(defclass sharp-book ()
  ((archive :initarg :archive :reader sb2-archive)
   (names   :initarg :names   :reader sb2-names)
   (entries :initarg :entries :reader sb2-entries)))

(defun %sharp-image-entries (archive)
  (let ((acc '()))
    (dolist (e (%enum->list (dotnet:invoke archive "get_Entries")))
      (unless (dotnet:invoke e "get_IsDirectory")
        (let ((key (dotnet:invoke e "get_Key")))
          (when (and key (image-name-p key)) (push (cons key e) acc)))))
    (sort (nreverse acc) #'natural< :key #'car)))

(defun make-sharp-book (path)
  ;; Open(string, ReaderOptions=null): 省略可引数を明示 nil で渡す (#292 回避)。
  (let* ((archive (dotnet:static "SharpCompress.Archives.ArchiveFactory" "Open" path nil))
         (pairs (%sharp-image-entries archive)))
    (make-instance 'sharp-book
                   :archive archive
                   :names   (coerce (mapcar #'car pairs) 'vector)
                   :entries (coerce (mapcar #'cdr pairs) 'vector))))

(defmethod book-page-count ((b sharp-book)) (length (sb2-entries b)))
(defmethod book-page-name  ((b sharp-book) i) (%basename (aref (sb2-names b) i)))
(defmethod book-load-image ((b sharp-book) i)
  (let ((es (dotnet:invoke (aref (sb2-entries b) i) "OpenEntryStream"))
        (ms (dotnet:new "System.IO.MemoryStream")))
    (unwind-protect
         (progn (dotnet:invoke es "CopyTo" ms)
                (dotnet:invoke ms "set_Position" 0)
                ;; #294 修正済 (0.1.12): 直呼びで Bitmap(Stream) を選べる。
                (dotnet:new "Avalonia.Media.Imaging.Bitmap" ms))
      (dotnet:invoke es "Dispose")
      (dotnet:invoke ms "Dispose"))))
(defmethod book-close ((b sharp-book))
  (ignore-errors (dotnet:invoke (sb2-archive b) "Dispose")))

;;; pdf-book (PDFtoImage/PDFium 経由) -------------------------------
;;; 各ページを C# シム Paalam.PdfDoc でラスタライズして Avalonia Bitmap に。

(defparameter *pdf-dpi* 300
  "PDF ページのラスタライズ解像度 (DPI)。150 固定だと 4K で拡大され粗いので可変に (#2)。
高いほど精細だがメモリ/時間増。4K は 300〜400 程度が目安。変更時は本を開き直して再描画。")

(defclass pdf-book ()
  ((doc   :initarg :doc   :reader pdfb-doc)
   (count :initarg :count :reader pdfb-count)))

(defun make-pdf-book (path)
  (let* ((doc (dotnet:new "Paalam.PdfDoc" path))
         (n   (dotnet:invoke doc "get_PageCount")))
    (make-instance 'pdf-book :doc doc :count n)))

(defmethod book-page-count ((b pdf-book)) (pdfb-count b))
(defmethod book-page-name  ((b pdf-book) i) (format nil "p~d" (1+ i)))
(defmethod book-load-image ((b pdf-book) i) (dotnet:invoke (pdfb-doc b) "RenderPage" i *pdf-dpi*))
(defmethod book-close      ((b pdf-book)) nil)

;;; book-page-bytes: 素材掃き出し (#18) 用にページの生バイトと拡張子を返す。
;;; (values byte[] extension)。dir/zip/sharp は格納形式そのまま、pdf は PNG 化。
(defgeneric book-page-bytes (book index))

(defun %name-ext (name) (string-downcase (or (pathname-type name) "dat")))

(defmethod book-page-bytes ((b directory-book) i)
  (let ((path (aref (db-paths b) i)))
    (values (dotnet:static "System.IO.File" "ReadAllBytes" path) (%name-ext path))))

(defmethod book-page-bytes ((b zip-book) i)
  (let ((es (dotnet:invoke (aref (zb-entries b) i) "Open"))
        (ms (dotnet:new "System.IO.MemoryStream")))
    (unwind-protect
         (progn (dotnet:invoke es "CopyTo" ms)
                (values (dotnet:invoke ms "ToArray") (%name-ext (aref (zb-names b) i))))
      (dotnet:invoke es "Dispose") (dotnet:invoke ms "Dispose"))))

(defmethod book-page-bytes ((b sharp-book) i)
  (let ((es (dotnet:invoke (aref (sb2-entries b) i) "OpenEntryStream"))
        (ms (dotnet:new "System.IO.MemoryStream")))
    (unwind-protect
         (progn (dotnet:invoke es "CopyTo" ms)
                (values (dotnet:invoke ms "ToArray") (%name-ext (aref (sb2-names b) i))))
      (dotnet:invoke es "Dispose") (dotnet:invoke ms "Dispose"))))

(defmethod book-page-bytes ((b pdf-book) i)
  (values (dotnet:invoke (pdfb-doc b) "RenderPagePng" i *pdf-dpi*) "png"))

;;; open-book --------------------------------------------------------

(defun open-book (path)
  "PATH (ディレクトリ / アーカイブ / 画像ファイル) を本として開く。
2 値 (book start-index) を返す。"
  (cond
    ;; 未指定 (path = nil): 空の本を開く (本未オープン状態の窓)。
    ((null path)
     (values (make-instance 'directory-book :paths #()) 0))
    ((dotnet:static "System.IO.Directory" "Exists" path)
     (values (make-directory-book path) 0))
    ((dotnet:static "System.IO.File" "Exists" path)
     (let ((ext (pathname-type path)))
       (cond
         ((and ext (member (string-downcase ext) '("zip" "cbz") :test #'string=))
          (values (make-zip-book path) 0))
         ((and ext (member (string-downcase ext)
                           '("rar" "cbr" "7z" "cb7" "tar" "cbt") :test #'string=))
          (values (make-sharp-book path) 0))
         ((and ext (string= (string-downcase ext) "pdf"))
          (values (make-pdf-book path) 0))
         ((image-ext-p ext)
          (let* ((dir  (directory-namestring path))
                 (book (make-directory-book dir))
                 (start (or (position (file-namestring path) (db-paths book)
                                      :key #'file-namestring :test #'string-equal)
                            0)))
            (values book start)))
         (t (error "対応していないファイル形式: ~a" path)))))
    (t (error "パスが見つかりません: ~a" path))))

;;; ------------------------------------------------------------------
;;; viewer: book + 現在ページ + UI 結線
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
(defparameter *settings*
  '((:image-direction   . *image-direction*)
    (:pdf-direction     . *pdf-direction*)
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

;;; 素材クリップ (#18 Phase 1): ((source . page) ...) 追加順。zip 掃き出し用。
(defparameter *clip* nil)
;; サムネのメモリキャッシュ (source.page -> Bitmap)。ペインを開くたびに source から
;; 再取得しないため (NAS で効く)。loader の cache とは別物 (loader は evict で Dispose
;; するので共有しない)。
(defparameter *thumb-cache* (make-hash-table :test 'equal))
(defun %thumb-get (src page) (gethash (cons src page) *thumb-cache*))
(defun %thumb-put (src page bmp) (setf (gethash (cons src page) *thumb-cache*) bmp))
(defun load-clip () (setf *clip* (%read-state "clip.lisp")))
(defun save-clip () (%write-state "clip.lisp" *clip*))
(defun clip-add (source page)
  "素材に (source . page) を追加 (重複はスキップ)。"
  (when (and source
             (not (member-if (lambda (e) (and (string-equal (car e) source) (eql (cdr e) page)))
                             *clip*)))
    (setf *clip* (append *clip* (list (cons source page))))
    (save-clip)))
(defun clip-clear () (setf *clip* nil) (clrhash *thumb-cache*) (save-clip))

(defun %sanitize (s)
  (map 'string (lambda (c) (if (or (alphanumericp c) (member c '(#\- #\_ #\.))) c #\_))
       (or s "x")))

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
                   (let* ((src (car item)) (page (cdr item))
                          (book (or (gethash src books)
                                    (setf (gethash src books) (open-book src)))))
                     (multiple-value-bind (bytes ext) (book-page-bytes book page)
                       (let* ((nm (format nil "~4,'0d_~a.~a" seq
                                          (%sanitize (pathname-name (book-page-name book page))) ext))
                              (entry (dotnet:invoke zip "CreateEntry" nm))
                              (es (dotnet:invoke entry "Open")))
                         (unwind-protect
                              (dotnet:invoke es "Write" bytes 0 (dotnet:invoke bytes "get_Length"))
                           (dotnet:invoke es "Dispose")))))
                 (error (e) (log-err "export-clip-item" e)))))
        (when zip (ignore-errors (dotnet:invoke zip "Dispose")))
        (ignore-errors (dotnet:invoke fs "Dispose"))
        (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (book-close v))) books))
      out)))

;;; 素材ペイン (#18 Phase 2): 集めたページをサムネ一覧で表示。サムネは背景
;;; スレッドで非同期ロードして後から差し込む (NAS でも UI を固めない)。

(defun %clip-remove-at (vw idx)
  "idx 番目の素材を取り除いてペイン更新。"
  (setf *clip* (loop for e in *clip* for i from 0 unless (= i idx) collect e))
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
        (dotnet:invoke children "Add" cell)))
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
  (let* ((pane (viewer-clip-pane vw))
         (vis (dotnet:invoke pane "get_IsVisible")))
    (dotnet:invoke pane "set_IsVisible" (not vis))
    (unless vis (%rebuild-clip-pane vw))))

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
  (%update-title vw)
  (when (dotnet:invoke (viewer-clip-pane vw) "get_IsVisible") (%rebuild-clip-pane vw)))

;;; ------------------------------------------------------------------
;;; ドラッグ&ドロップ (#302): C# シム (Paalam.DragDropHelper) を剥がし Lisp 直叩き。
;;; Avalonia 12 の DnD は IDataTransfer + DataFormat。ルーテッドイベントは静的
;;; RoutedEvent フィールド (DragDrop.DropEvent 等) を読み、EventHandler<DragEventArgs>
;;; デリゲートで AddHandler 購読する。ファイル取り出し/パス化は静的拡張メソッド
;;; (DataTransferExtensions / StorageProviderExtensions) を直叩き。
;;; ------------------------------------------------------------------

(defun %dnd-has-files (e)
  "DragEventArgs e がファイルを含むか (DataTransferExtensions.Contains(_, DataFormat.File))。"
  (handler-case
      (let ((dt (dotnet:invoke e "get_DataTransfer")))
        (and dt
             (dotnet:static "Avalonia.Input.DataTransferExtensions" "Contains"
                            dt (dotnet:static "Avalonia.Input.DataFormat" "get_File"))
             t))
    (error (err) (log-err "dnd-has-files" err) nil)))

(defun %dnd-paths (e)
  "DragEventArgs e からドロップされたローカルパスのリストを取り出す。"
  (handler-case
      (let ((dt (dotnet:invoke e "get_DataTransfer"))
            (paths '()))
        (when dt
          (let ((files (dotnet:static "Avalonia.Input.DataTransferExtensions" "TryGetFiles" dt)))
            (when files
              (let ((en (dotnet:invoke files "GetEnumerator")))
                (loop while (dotnet:invoke en "MoveNext")
                      do (let* ((item (dotnet:invoke en "get_Current"))
                                (p (and item (dotnet:static
                                              "Avalonia.Platform.Storage.StorageProviderExtensions"
                                              "TryGetLocalPath" item))))
                           (when (and p (> (length p) 0)) (push p paths))))))))
        (nreverse paths))
    (error (err) (log-err "dnd-paths" err) nil)))

(defun %dnd-add-handler (target event-name handler &optional handled-too)
  "target (Interactive) の静的 RoutedEvent (DragDrop の event-name フィールド) に
handler=(lambda (sender args)) を購読。
AddHandler(RoutedEvent, EventHandler<DragEventArgs>, RoutingStrategies, bool)。"
  ;; 構築済みジェネリック デリゲート型は型引数を [[Type, Assembly]] と
  ;; アセンブリ修飾しないと dotcl が解決できない (単純 [Type] は corelib 型のみ; #302)。
  (dotnet:invoke target "AddHandler"
                 (dotnet:static "Avalonia.Input.DragDrop" event-name)
                 (dotnet:make-delegate
                  "System.EventHandler`1[[Avalonia.Input.DragEventArgs, Avalonia.Base]]"
                  handler)
                 "Direct, Bubble"
                 (and handled-too t)))

(defun %add-pressed-tunnel (target handler)
  "PointerPressed を Tunnel + handledEventsToo で購読 (handler=(lambda (sender args)))。
TreeViewItem 等が選択処理で PointerPressed を Handled にしても先に受け取れる
(intra-app ドラッグ開始トリガ用; #32)。CLR の += (add-event) は handledEventsToo=false
なので捕捉済みイベントを取りこぼす。"
  (dotnet:invoke target "AddHandler"
                 (dotnet:static "Avalonia.Input.InputElement" "PointerPressedEvent")
                 (dotnet:make-delegate
                  "System.EventHandler`1[[Avalonia.Input.PointerPressedEventArgs, Avalonia.Base]]"
                  handler)
                 "Tunnel"
                 t))

(defun %dnd-set-effect (e has)
  (dotnet:invoke e "set_DragEffects"
                 (dotnet:static "Avalonia.Input.DragDropEffects" (if has "Copy" "None"))))

(defun %dnd-enable (win on-drop on-highlight)
  "ウィンドウへ本ファイルの DnD を配線。on-drop=(lambda (path))、on-highlight=(lambda (bool))。
ファイルが来たら Copy エフェクト、ドロップで先頭パスを on-drop に渡す。"
  (dotnet:static "Avalonia.Input.DragDrop" "SetAllowDrop" win t)
  (%dnd-add-handler win "DragOverEvent"
                    (lambda (s e) (declare (ignore s))
                      (let ((has (%dnd-has-files e)))
                        (%dnd-set-effect e has)
                        (funcall on-highlight has))))
  (%dnd-add-handler win "DragLeaveEvent"
                    (lambda (s e) (declare (ignore s e)) (funcall on-highlight nil)))
  (%dnd-add-handler win "DropEvent"
                    (lambda (s e) (declare (ignore s))
                      (funcall on-highlight nil)
                      (let ((paths (%dnd-paths e)))
                        (when paths (funcall on-drop (first paths)))))))

(defun %dnd-enable-clip (pane on-drop on-highlight)
  "素材ペインへ DnD を配線。落ちた全パスを on-drop に渡し、e.Handled=t で
ウィンドウ側の「本を開く」DnD と区別する。"
  (dotnet:static "Avalonia.Input.DragDrop" "SetAllowDrop" pane t)
  (%dnd-add-handler pane "DragOverEvent"
                    (lambda (s e) (declare (ignore s))
                      (let ((has (%dnd-has-files e)))
                        (%dnd-set-effect e has)
                        (funcall on-highlight has)
                        (dotnet:invoke e "set_Handled" t))))
  (%dnd-add-handler pane "DragLeaveEvent"
                    (lambda (s e) (declare (ignore s))
                      (funcall on-highlight nil)
                      (dotnet:invoke e "set_Handled" t)))
  (%dnd-add-handler pane "DropEvent"
                    (lambda (s e) (declare (ignore s))
                      (funcall on-highlight nil)
                      (dolist (p (%dnd-paths e)) (funcall on-drop p))
                      (dotnet:invoke e "set_Handled" t))))

;;; ------------------------------------------------------------------
;;; ページサムネ本棚 (#6 A): サイドペインに現在の本の全ページを縮小サムネで並べ、
;;; クリックでそのページへジャンプ。サムネは DecodeToWidth で縮小デコード (全ページ分
;;; フル Bitmap を抱えないため)。背景スレッドで非同期ロードして後から差し込む。
;;; ------------------------------------------------------------------
(defparameter *thumb-width* 150 "ページサムネの幅 (px)。")
(defparameter *page-thumb-cache* (make-hash-table :test 'equal)
  "(source . page) -> 縮小サムネ Bitmap。本を変えたらクリア。")
(defun %pthumb-get (src page) (gethash (cons src page) *page-thumb-cache*))
(defun %pthumb-put (src page bmp) (setf (gethash (cons src page) *page-thumb-cache*) bmp))
(defun %pthumb-clear ()
  (maphash (lambda (k v) (declare (ignore k)) (ignore-errors (dotnet:invoke v "Dispose")))
           *page-thumb-cache*)
  (clrhash *page-thumb-cache*))

(defun %make-page-thumb (book index width)
  "BOOK の INDEX ページを WIDTH px に縮小デコードした Bitmap。バイト列から DecodeToWidth。
MemoryStream(byte[]) ctor は overload バグ (#294) を踏むので無引数 + Write で渡す。"
  (multiple-value-bind (bytes ext) (book-page-bytes book index)
    (declare (ignore ext))
    (let ((ms (dotnet:new "System.IO.MemoryStream")))
      (unwind-protect
           (progn
             (dotnet:invoke ms "Write" bytes 0 (dotnet:invoke bytes "get_Length"))
             (dotnet:invoke ms "set_Position" 0)
             (dotnet:static "Avalonia.Media.Imaging.Bitmap" "DecodeToWidth" ms width))
        (dotnet:invoke ms "Dispose")))))

(defun %jump-to-page (vw page)
  "PAGE を含む見開きへ移動 (本棚クリック)。フォーカスはビューポートへ戻す。"
  (setf (viewer-sindex vw) (spread-index-of (viewer-spreads vw) page))
  (show-spread vw)
  (%focus-viewport vw))

(defun %rebuild-thumb-pane (vw)
  "thumb-stack を現在の本の全ページサムネで作り直す。キャッシュ済みは即表示、
未キャッシュは背景スレッドで縮小デコードして差し込む (本は別ハンドルで開き直す)。"
  (let* ((stack (viewer-thumb-stack vw))
         (children (dotnet:invoke stack "get_Children"))
         (src (viewer-source vw))
         (n (book-page-count (viewer-book vw)))
         (pending '()))
    (dotnet:invoke children "Clear")
    (dotimes (i n)
      (let ((img (dotnet:new "Avalonia.Controls.Image"))
            (page i)
            (cached (%pthumb-get src i)))
        (dotnet:invoke img "set_Width" (coerce *thumb-width* 'double-float))
        (dotnet:invoke img "set_Stretch" (dotnet:static "Avalonia.Media.Stretch" "Uniform"))
        (dotnet:invoke img "set_Margin" (dotnet:new "Avalonia.Thickness" 4d0 3d0 4d0 3d0))
        (dotnet:add-event img "PointerPressed"
          (lambda (s e) (declare (ignore s))
            (dotnet:invoke e "set_Handled" t)   ; ウィンドウのパンを起こさない
            (%jump-to-page vw page)))
        (if cached
            (dotnet:invoke img "set_Source" cached)
            (push (cons img i) pending))
        (dotnet:invoke children "Add" img)))
    (when pending
      (setf pending (nreverse pending))
      (make-thread
       (lambda ()
         (let ((book (ignore-errors (open-book src))))
           (when book
             (unwind-protect
                  (dolist (pair pending)
                    (ignore-errors
                      (let* ((img (car pair)) (page (cdr pair))
                             (bmp (%make-page-thumb book page *thumb-width*)))
                        (when bmp
                          (%pthumb-put src page bmp)
                          (%ui-post (lambda () (dotnet:invoke img "set_Source" bmp)))))))
               (ignore-errors (book-close book))))))
       :name "paalam-page-thumbs"))))

(defun %toggle-thumb-pane (vw)
  (let* ((pane (viewer-thumb-pane vw))
         (vis (dotnet:invoke pane "get_IsVisible")))
    (dotnet:invoke pane "set_IsVisible" (not vis))
    (unless vis (%rebuild-thumb-pane vw))))

(defun %thumb-pane-refresh (vw)
  "本を変えたとき: サムネ表示を作り直し、旧サムネ Bitmap は描画後に破棄 (use-after-dispose 回避)。"
  (let ((old '()))
    (maphash (lambda (k v) (declare (ignore k)) (push v old)) *page-thumb-cache*)
    (clrhash *page-thumb-cache*)   ; 新しい本は別キーなので参照だけ落とす
    (when (and (viewer-thumb-pane vw)
               (dotnet:invoke (viewer-thumb-pane vw) "get_IsVisible"))
      (%rebuild-thumb-pane vw))    ; 旧 Image を tstack から外す (Clear)
    (when old
      (%defer (lambda () (dolist (b old) (ignore-errors (dotnet:invoke b "Dispose"))))))))

;;; ------------------------------------------------------------------
;;; フォルダツリー本棚 (#6 B): サイドにファイルシステムのツリー。ドライブをルートに
;;; 遅延展開 (選択でその直下を読む)。クリック=展開ナビ、ダブルクリック=本として開く
;;; (クリックだけで巨大フォルダを recursive 展開しないように分ける)。
;;; ------------------------------------------------------------------
(defparameter *book-file-extensions*
  '("zip" "cbz" "rar" "cbr" "7z" "cb7" "tar" "cbt" "pdf")
  "ツリーで本として扱うファイル拡張子 (フォルダ + これら)。")
(defun %book-file-p (name)
  (let ((ext (pathname-type name)))
    (and ext (member (string-downcase ext) *book-file-extensions* :test #'string=) t)))

(defun %dir-children (dir)
  "DIR 直下の (label . fullpath): サブフォルダ(自然順) → 本ファイル(自然順)。アクセス不可は無視。"
  (let ((folders '()) (files '()))
    (ignore-errors
      (dolist (d (%enum->list (dotnet:static "System.IO.Directory" "GetDirectories" dir)))
        (push d folders)))
    (ignore-errors
      (dolist (f (%enum->list (dotnet:static "System.IO.Directory" "GetFiles" dir)))
        (when (%book-file-p f) (push f files))))
    (append (mapcar (lambda (p) (cons (%basename p) p)) (sort folders #'natural<))
            (mapcar (lambda (p) (cons (%basename p) p)) (sort files #'natural<)))))

(defun %browse-tvi (label path)
  (let ((it (dotnet:new "Avalonia.Controls.TreeViewItem")))
    (dotnet:invoke it "set_Header" label)
    (dotnet:invoke it "set_Tag" path)   ; Tag=フルパス (文字列なので marshal は素直)
    it))

(defun %browse-populate (item dir)
  "ITEM (フォルダノード) の子を未展開なら DIR 直下で埋める。"
  (let ((items (dotnet:invoke item "get_Items")))
    (when (zerop (dotnet:invoke items "get_Count"))
      (dolist (e (%dir-children dir))
        (dotnet:invoke items "Add" (%browse-tvi (car e) (cdr e)))))))

(defun %rebuild-browse-pane (vw)
  "ルート (論理ドライブ) を並べる。配下は選択時に遅延展開。"
  (let ((items (dotnet:invoke (viewer-browse-tree vw) "get_Items")))
    (dotnet:invoke items "Clear")
    (ignore-errors
      (dolist (d (%enum->list (dotnet:static "System.IO.Directory" "GetLogicalDrives")))
        (dotnet:invoke items "Add" (%browse-tvi d d))))))

(defun %path-ancestors (path)
  "PATH の祖先フルパス列を ルート(ドライブ)→PATH の順で。"
  (let ((acc (list path)) (cur path))
    (loop
      (let ((parent (ignore-errors (dotnet:static "System.IO.Path" "GetDirectoryName" cur))))
        (if (and parent (plusp (length parent)) (not (string-equal parent cur)))
            (progn (push parent acc) (setf cur parent))
            (return))))
    acc))

(defun %browse-find-child (items path)
  "ITEMS (TreeViewItem の Items) から Tag=PATH のノードを探す (大小無視)。"
  (let ((n (dotnet:invoke items "get_Count")))
    (dotimes (i n)
      (let ((it (dotnet:invoke items "get_Item" i)))
        (when (string-equal (or (dotnet:invoke it "get_Tag") "") path)
          (return-from %browse-find-child it))))))

(defun %browse-reveal (vw target)
  "ドライブから TARGET まで辿って各ノードを展開し、最深まで届いたノードを選択+可視化。
TARGET がツリーに出ない種別 (素の画像等) なら、辿れた最後のフォルダを選ぶ。"
  (when (and target (plusp (length target)))
    (let ((items (dotnet:invoke (viewer-browse-tree vw) "get_Items")) (node nil))
      (dolist (a (%path-ancestors target))
        (let ((child (%browse-find-child items a)))
          (unless child (return))
          (setf node child)
          (when (dotnet:static "System.IO.Directory" "Exists" a)
            (%browse-populate node a)
            (dotnet:invoke node "set_IsExpanded" t))
          (setf items (dotnet:invoke node "get_Items"))))
      (when node
        (dotnet:invoke node "set_IsSelected" t)
        (ignore-errors (dotnet:invoke node "BringIntoView"))))))

(defun %toggle-browse-pane (vw)
  (let* ((pane (viewer-browse-pane vw))
         (vis (dotnet:invoke pane "get_IsVisible")))
    (dotnet:invoke pane "set_IsVisible" (not vis))
    (unless vis
      (when (zerop (dotnet:invoke (dotnet:invoke (viewer-browse-tree vw) "get_Items") "get_Count"))
        (%rebuild-browse-pane vw))
      ;; 開いている本のディレクトリまで展開・選択する。
      (ignore-errors (%browse-reveal vw (viewer-source vw))))))

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

(defun open-loader (path on-loaded)
  "PATH を *loader-threads* スレッドで並列ロードするローダーを起動。
並列安全な book は 1 ハンドル共有、そうでなければスレッド毎に別ハンドルを開く。"
  (let* ((n (max 1 *loader-threads*))
         (first (open-book path))
         (nh (if (book-parallel-safe-p first) 1 n))   ; ハンドル数
         (books (coerce (cons first (loop repeat (1- nh) collect (open-book path))) 'vector))
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
  "source を開き直して book/loader/spreads を差し替える (再帰トグル等)。現在ページは維持。"
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
                                (viewer-source vw)
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
                                      path
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
         (clip (if *clip* (format nil " [素材~d]" (length *clip*)) ""))
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
                        dir-img-combo dir-pdf-combo theme-combo)
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
                            (list "PDF 解像度 (DPI)" tb-dpi (reset-text tb-dpi :pdf-dpi)))))
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
                                 dir-img-combo dir-pdf-combo theme-combo)
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

;;; メニューバー (#21)
;;; Menu/MenuItem はキーボードフォーカスを保持する。クリック後フォーカスが
;;; メニューに残ると、矢印/Space/Esc 等がメニューのナビに食われて viewer の
;;; KeyDown まで届かない。そこでクリック後は必ずビューポートへフォーカスを戻す。
(defun %focus-viewport (vw)
  (ignore-errors (dotnet:invoke (viewer-viewport vw) "Focus")))

(defun %menu-item (vw header &optional click)
  (let ((mi (dotnet:new "Avalonia.Controls.MenuItem")))
    (dotnet:invoke mi "set_Header" header)
    (when click
      (dotnet:add-event mi "Click"
        (lambda (s e) (declare (ignore s e)) (funcall click) (%focus-viewport vw))))
    mi))
(defun %menu-add (parent item) (dotnet:invoke (dotnet:invoke parent "get_Items") "Add" item))

(defun %key-label (k)
  "Avalonia Key 名をメニュー表示用に整える ([ ] 0 等)。"
  (cond ((null k) nil)
        ((or (string= k "D0") (string= k "NumPad0")) "0")
        ((string= k "OemOpenBrackets") "[")
        ((string= k "OemCloseBrackets") "]")
        ((string= k "OemSemicolon") ":")
        ((string= k "OemComma") ",")
        ((string= k "OemPeriod") ".")
        (t k)))

(defun %menu-cmd (vw parent id)
  "コマンド ID をメニュー項目として PARENT に追加 (ラベル + 代表キーを表示)。"
  (let* ((c (%command id))
         (k (%key-label (%command-primary-key id)))
         (header (if k (format nil "~a (~a)" (command-label c) k) (command-label c))))
    (%menu-add parent (%menu-item vw header (lambda () (%run-command vw id))))))

;;; メニュー内セパレータ (#32)。区切りは一意な cons (:sep) で表す (識別子衝突を避け、
;;; 並べ替え/削除を eql 同一性で扱えるようにするため)。menu.lisp には (:SEP) で round-trip。
(defun %make-menu-sep () (list :sep))
(defun %menu-sep-p (x) (and (consp x) (eq (car x) :sep)))
(defun %fresh-gkey (menu)
  "MENU (グループ列; 各要素の first が gkey) に無い新しいグループキーワードを返す。"
  (loop for i from 1
        for k = (intern (format nil "GROUP-~d" i) :keyword)
        unless (find k menu :key #'first) return k))
(defun %menu-render-entries (ids)
  "IDS (コマンド id とセパレータの混在) を描画用に整える: 実在コマンドとセパレータだけにし、
先頭/末尾/連続したセパレータは落とす。返り値はコマンド id と :sep マーカーの並び。"
  (let ((out '()) (pending nil) (seen nil))
    (dolist (id ids)
      (cond ((%menu-sep-p id) (when seen (setf pending t)))
            ((%command id)
             (when pending (push :sep out) (setf pending nil))
             (push id out) (setf seen t))))
    (nreverse out)))

(defun %add-menu-groups (vw container)
  "CONTAINER (Menu / ContextMenu) に *command-menu* の順でグループ submenu を追加し、
各グループに該当コマンドとセパレータを並べる (メニュー/コンテキスト共通; #26/#32)。"
  (let ((items (dotnet:invoke container "get_Items")))
    (dolist (g *command-menu*)
      (destructuring-bind (gkey title &rest ids) g
        (declare (ignore gkey))
        (let ((entries (%menu-render-entries ids)))
          (when entries        ; 実コマンドが 1 つ以上ある時のみグループを出す
            (let ((sub (%menu-item vw title)))
              (dolist (e entries)
                (if (eq e :sep)
                    (%menu-add sub (dotnet:new "Avalonia.Controls.Separator"))
                    (%menu-cmd vw sub e)))
              (dotnet:invoke items "Add" sub))))))))

(defun %build-menu (vw win)
  (declare (ignore win))   ; 開く系コマンドは (viewer-window vw) を owner に使う
  (let ((menu (dotnet:new "Avalonia.Controls.Menu")))
    (%add-menu-groups vw menu)
    menu))

(defun %build-context-menu (vw)
  "ビューポート右クリック用。メニューと同じ *command-menu* 定義から作る (#26)。
ジェスチャ発火直後 (右ドラッグでコマンド実行) は Opening を Cancel して開かない (#29)。
ContextMenu は set_ContextMenu 時に自前で ContextRequested を購読するため、
外側で ContextRequested を Handled にしても止まらない → Opening.Cancel で止める。"
  (let ((cm (dotnet:new "Avalonia.Controls.ContextMenu")))
    (%add-menu-groups vw cm)
    (dotnet:add-event cm "Opening"
                      (lambda (s e) (declare (ignore s))
                        (when (viewer-gesture-fired vw)
                          (dotnet:invoke e "set_Cancel" t)
                          (setf (viewer-gesture-fired vw) nil))))
    cm))

(defun %rebuild-menus (vw)
  "*command-menu* からメニューバーとコンテキストメニューを作り直す (#26 編集の即時反映)。"
  (when (viewer-menu vw)
    (dotnet:invoke (dotnet:invoke (viewer-menu vw) "get_Items") "Clear")
    (%add-menu-groups vw (viewer-menu vw)))
  (ignore-errors
    (dotnet:invoke (viewer-viewport vw) "set_ContextMenu" (%build-context-menu vw))))

;;; メニュー編集 (#26)。グループ内の並べ替え/削除/追加 → menu.lisp 永続化 + 即反映。
(defun %shift-in-list (lst item dir)
  "LST 内の ITEM を dir (-1 上 / +1 下) 方向に隣と入れ替えた新リスト。"
  (let ((v (copy-list lst)) (pos (position item lst)))
    (when pos
      (let ((j (+ pos dir)))
        (when (and (>= j 0) (< j (length v))) (rotatef (nth pos v) (nth j v)))))
    v))
(defun %insert-before (ids item before)
  "IDS に ITEM を BEFORE の直前へ挿入した新リスト (BEFORE 不在なら末尾)。
ITEM は呼び側で除去済み前提 (重複は作らない)。"
  (if (member before ids)
      (loop for x in ids
            when (eql x before) collect item
            collect x)
      (append ids (list item))))
(defun %menu-norm (menu)   ; (gkey title id...) → (gkey title (id...)) 編集用コピー
  (mapcar (lambda (g) (list (first g) (second g) (copy-list (cddr g)))) menu))
(defun %menu-denorm (menu) ; 逆変換
  (mapcar (lambda (g) (list* (first g) (second g) (third g))) menu))

;;; マウスジェスチャ編集 (#29)。記録ボックスでドラッグ→方向列、コマンドを選んで
;;; 追加/更新。gestures.lisp に即保存 (*gestures* はジェスチャ時に直接参照される)。
(defun %gesture-label (id)
  (let ((c (%command id))) (if c (command-label c) (format nil "~a" id))))

(defun %open-gesture-editor (vw)
  "マウスジェスチャ割当の編集ウィンドウ (#29)。記録ボックスでドラッグして方向列を取り、
コマンドを選んで「追加/更新」。既存はリストから削除、既定に戻すも可。"
  (let* ((owner (viewer-window vw))
         (w    (dotnet:new "Avalonia.Controls.Window"))
         (cap  (dotnet:new "Avalonia.Controls.Border"))
         (cap-text  (dotnet:new "Avalonia.Controls.TextBlock"))
         (lst  (dotnet:new "Avalonia.Controls.StackPanel"))
         (choices (mapcar (lambda (c) (cons (command-label c) (command-id c))) *commands*))
         (combo (%s-choice-combo choices (command-id (first *commands*))))
         (capbox (list "" 0d0 0d0 nil)))   ; (seq lx ly active)
    (labels
        ((cap-show ()
           (dotnet:invoke cap-text "set_Text"
                          (let ((s (first capbox)))
                            (if (plusp (length s)) (format nil "記録: ~a" s)
                                "ここでドラッグして方向を記録 (U↑ D↓ L← R→)"))))
         (refresh ()
           (let ((items (dotnet:invoke lst "get_Children")))
             (dotnet:invoke items "Clear")
             (dolist (g *gestures*)
               (let* ((seq (car g)) (id (cdr g))
                      (row (dotnet:new "Avalonia.Controls.DockPanel"))
                      (lbl (dotnet:new "Avalonia.Controls.TextBlock"))
                      (del (%s-button "削除" (let ((s seq)) (lambda () (del-gesture s))))))
                 (dotnet:invoke lbl "set_Text" (format nil "~a  →  ~a" seq (%gesture-label id)))
                 (dotnet:invoke lbl "set_VerticalAlignment"
                                (dotnet:static "Avalonia.Layout.VerticalAlignment" "Center"))
                 (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" del
                                (dotnet:static "Avalonia.Controls.Dock" "Right"))
                 (let ((c (dotnet:invoke row "get_Children")))
                   (dotnet:invoke c "Add" del) (dotnet:invoke c "Add" lbl))
                 (dotnet:invoke row "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 2d0 0d0 2d0))
                 (dotnet:invoke items "Add" row)))))
         (del-gesture (seq)
           (setf *gestures* (remove seq *gestures* :key #'car :test #'string-equal))
           (save-gestures) (refresh))
         (add-gesture ()
           (let ((seq (first capbox)) (id (%s-combo-value combo choices)))
             (when (and (plusp (length seq)) id)
               ;; 同じ方向列の既存割当は置き換え (先頭に積む)。
               (setf *gestures* (cons (cons seq id)
                                      (remove seq *gestures* :key #'car :test #'string-equal)))
               (save-gestures) (refresh))))
         (reset-default ()
           (setf *gestures* (copy-tree *default-gestures*)) (save-gestures) (refresh)))
      (dotnet:invoke cap "set_Height" 90d0)
      (dotnet:invoke cap "set_Background" (dotnet:static "Avalonia.Media.Brushes" "DimGray"))
      (dotnet:invoke cap-text "set_HorizontalAlignment"
                     (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Center"))
      (dotnet:invoke cap-text "set_VerticalAlignment"
                     (dotnet:static "Avalonia.Layout.VerticalAlignment" "Center"))
      (dotnet:invoke cap "set_Child" cap-text)
      (cap-show)
      (dotnet:add-event cap "PointerPressed"
                        (lambda (s e) (declare (ignore s))
                          (multiple-value-bind (x y) (%pos-xy e cap)
                            (setf (first capbox) "" (second capbox) x (third capbox) y
                                  (fourth capbox) t)
                            (cap-show))))
      (dotnet:add-event cap "PointerMoved"
                        (lambda (s e) (declare (ignore s))
                          (when (fourth capbox)
                            (multiple-value-bind (x y) (%pos-xy e cap)
                              (multiple-value-bind (seq lx ly moved)
                                  (%gesture-step (first capbox) (second capbox) (third capbox) x y)
                                (when moved
                                  (setf (first capbox) seq (second capbox) lx (third capbox) ly)
                                  (cap-show)))))))
      (dotnet:add-event cap "PointerReleased"
                        (lambda (s e) (declare (ignore s e))
                          (setf (fourth capbox) nil) (cap-show)))
      (refresh)
      (let* ((root (dotnet:new "Avalonia.Controls.StackPanel"))
             (addrow (dotnet:new "Avalonia.Controls.StackPanel"))
             (btns (dotnet:new "Avalonia.Controls.StackPanel")))
        (dotnet:invoke addrow "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
        (dotnet:invoke addrow "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 8d0 0d0 0d0))
        (let ((c (dotnet:invoke addrow "get_Children")))
          (dotnet:invoke c "Add" combo)
          (dotnet:invoke c "Add" (%s-button "追加/更新" (lambda () (add-gesture)))))
        (dotnet:invoke btns "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
        (dotnet:invoke btns "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 12d0 0d0 0d0))
        (let ((c (dotnet:invoke btns "get_Children")))
          (dotnet:invoke c "Add" (%s-button "既定に戻す" (lambda () (reset-default))))
          (dotnet:invoke c "Add" (%s-button "閉じる" (lambda () (dotnet:invoke w "Close")))))
        (dotnet:invoke root "set_Margin" (dotnet:new "Avalonia.Thickness" 12d0))
        (let ((c (dotnet:invoke root "get_Children")))
          (dotnet:invoke c "Add" (%s-header "マウスジェスチャ (右ドラッグの方向列 → コマンド)"))
          (dotnet:invoke c "Add" cap)
          (dotnet:invoke c "Add" addrow)
          (dotnet:invoke c "Add" (%s-header "現在の割当"))
          (dotnet:invoke c "Add" lst)
          (dotnet:invoke c "Add" btns))
        (dotnet:invoke w "set_Content" root))
      (dotnet:invoke w "set_Title" "マウスジェスチャ編集")
      (dotnet:invoke w "set_Width" 400d0)
      (dotnet:invoke w "set_SizeToContent" (dotnet:static "Avalonia.Controls.SizeToContent" "Height"))
      (dotnet:invoke w "set_WindowStartupLocation"
                     (dotnet:static "Avalonia.Controls.WindowStartupLocation" "CenterOwner"))
      (dotnet:invoke w "ShowDialog" owner))))

(defun %open-menu-editor (vw)
  "メニュー構成の編集ウィンドウ (#32: NeeView 流 TreeView + ドラッグ&ドロップ + 右クリック)。
グループ=展開ノード / コマンド=子。コマンドをドラッグして別コマンド上に落とすとその直前へ、
グループ見出しに落とすとそのグループ末尾へ移動 (Avalonia intra-app DnD)。右クリックで
上へ/下へ/削除/別グループへ・グループに「コマンドを追加」。OK で *command-menu* を
menu.lisp に保存し即反映。"
  (let* ((owner (viewer-window vw))
         (w    (dotnet:new "Avalonia.Controls.Window"))
         (tree (dotnet:new "Avalonia.Controls.TreeView"))
         (mbox (list (%menu-norm *command-menu*)))
         (dstate (list nil))     ; ドラッグ中のコマンド (cons gk id)
         (dragging (list nil)))  ; 多重開始防止
    (labels
        ((groups () (car mbox))
         (upd (gk fn)
           (setf (car mbox)
                 (mapcar (lambda (g) (if (eq (first g) gk)
                                         (list (first g) (second g) (funcall fn (third g))) g))
                         (groups)))
           (refresh))
         (move-cmd (gk id dir) (upd gk (lambda (ids) (%shift-in-list ids id dir))))
         (remove-cmd (gk id)   (upd gk (lambda (ids) (remove id ids))))
         (add-cmd (gk id)      (upd gk (lambda (ids) (if (member id ids) ids (append ids (list id))))))
         (move-to-group (gk id newgk)
           (unless (eq gk newgk)
             (setf (car mbox)
                   (mapcar (lambda (g)
                             (cond ((eq (first g) gk)
                                    (list (first g) (second g) (remove id (third g))))
                                   ((eq (first g) newgk)
                                    (list (first g) (second g)
                                          (if (member id (third g)) (third g)
                                              (append (third g) (list id)))))
                                   (t g)))
                           (groups)))
             (refresh)))
         ;; --- グループ自体の操作 (#32: 追加/リネーム/削除/並べ替え + 区切り) ---
         (move-group (gk dir)            ; グループの順序を上(-1)/下(+1)へ
           (let* ((gs (groups)) (g (find gk gs :key #'first)))
             (when g (setf (car mbox) (%shift-in-list gs g dir)) (refresh))))
         (add-group ()                   ; 名前を聞いて末尾に空グループを足す
           (%prompt-text w "新規グループ名" ""
                         (lambda (name)
                           (setf (car mbox)
                                 (append (groups) (list (list (%fresh-gkey (groups)) name nil))))
                           (refresh))))
         (rename-group (gk)
           (let ((g (find gk (groups) :key #'first)))
             (when g
               (%prompt-text w "グループ名を変更" (second g)
                             (lambda (name)
                               (setf (car mbox)
                                     (mapcar (lambda (x) (if (eq (first x) gk)
                                                             (list (first x) name (third x)) x))
                                             (groups)))
                               (refresh))))))
         (delete-group (gk)
           (setf (car mbox) (remove gk (groups) :key #'first)) (refresh))
         (add-sep (gk)                   ; グループ末尾に区切り線を足す
           (upd gk (lambda (ids) (append ids (list (%make-menu-sep))))))
         ;; --- ドラッグ&ドロップ並べ替え (#32, Avalonia intra-app DnD) ---
         (drop-on-cmd (tgt-gk tgt-id)   ; コマンド行に落とす = その直前へ
           (let ((src (car dstate)))
             (when src
               (let ((sgk (car src)) (sid (cdr src)))
                 (unless (and (eq sgk tgt-gk) (eql sid tgt-id))
                   (setf (car mbox)
                         (mapcar (lambda (g)
                                   (let ((k (first g)) (ids (third g)))
                                     (cond
                                       ((and (eq k sgk) (eq k tgt-gk))
                                        (list k (second g) (%insert-before (remove sid ids) sid tgt-id)))
                                       ((eq k sgk)    (list k (second g) (remove sid ids)))
                                       ((eq k tgt-gk) (list k (second g) (%insert-before ids sid tgt-id)))
                                       (t g))))
                                 (groups)))
                   (refresh))))
             (setf (car dstate) nil)))
         (drop-on-grp (tgt-gk)          ; グループ見出しに落とす = 末尾へ移動
           (let ((src (car dstate)))
             (when src (move-to-group (car src) (cdr src) tgt-gk)))
           (setf (car dstate) nil))
         (left-pressed (item e)         ; その PointerPressed は左ボタンか
           (let ((props (dotnet:invoke (dotnet:invoke e "GetCurrentPoint" item) "get_Properties")))
             (dotnet:invoke props "get_IsLeftButtonPressed")))
         (start-drag (item gk id e)     ; 生きた PointerPressedEventArgs でその場でドラッグ開始
           ;; TreeViewItem は押下でポインタを捕捉するため PointerMoved 遅延開始は不可。
           ;; DoDragDropAsync は移動が無ければ何も起きない (純クリックは安全)。
           (unless (car dragging)
             (setf (car dragging) t (car dstate) (cons gk id))
             (let ((dt (dotnet:new "Avalonia.Input.DataTransfer")))
               (dotnet:invoke dt "Add"
                              (dotnet:static "Avalonia.Input.DataTransferItem" "CreateText" "paalam-menu"))
               (handler-case
                   (dotnet:static "Avalonia.Input.DragDrop" "DoDragDropAsync"
                                  e dt (dotnet:static "Avalonia.Input.DragDropEffects" "Move"))
                 (error (err) (log-err "menu-dnd" err))))
             (setf (car dragging) nil (car dstate) nil)))
         (setup-drag (item gk id)       ; コマンド行をドラッグ元に (左押下で開始)
           ;; Tunnel+handledEventsToo: TreeViewItem の選択処理に PointerPressed を
           ;; 食われる前に取る (CLR += では取りこぼす)。
           (%add-pressed-tunnel item
                                (lambda (s e) (declare (ignore s))
                                  (when (left-pressed item e) (start-drag item gk id e)))))
         (setup-drop (item on-drop)     ; ドロップ先 (コマンド行/グループ見出し)
           (dotnet:static "Avalonia.Input.DragDrop" "SetAllowDrop" item t)
           (%dnd-add-handler item "DragOverEvent"
                             (lambda (s e) (declare (ignore s))
                               (dotnet:invoke e "set_DragEffects"
                                              (dotnet:static "Avalonia.Input.DragDropEffects" "Move"))
                               (dotnet:invoke e "set_Handled" t))
                             t)
           (%dnd-add-handler item "DropEvent"
                             (lambda (s e) (declare (ignore s))
                               (funcall on-drop)
                               (dotnet:invoke e "set_Handled" t))
                             t))
         (mkmi (header thunk)            ; Click する MenuItem
           (let ((m (dotnet:new "Avalonia.Controls.MenuItem")))
             (dotnet:invoke m "set_Header" header)
             (dotnet:add-event m "Click" (lambda (s e) (declare (ignore s e)) (funcall thunk)))
             m))
         (mksub (header subitems)        ; サブメニュー MenuItem
           (let ((m (dotnet:new "Avalonia.Controls.MenuItem")))
             (dotnet:invoke m "set_Header" header)
             (let ((it (dotnet:invoke m "get_Items"))) (dolist (x subitems) (dotnet:invoke it "Add" x)))
             m))
         (ctx (items)                    ; ContextMenu
           (let ((cm (dotnet:new "Avalonia.Controls.ContextMenu")))
             (let ((it (dotnet:invoke cm "get_Items"))) (dolist (x items) (dotnet:invoke it "Add" x)))
             cm))
         (cmd-ctx (gk id)
           (ctx (list
                 (mkmi "上へ" (lambda () (move-cmd gk id -1)))
                 (mkmi "下へ" (lambda () (move-cmd gk id 1)))
                 (mkmi "削除" (lambda () (remove-cmd gk id)))
                 (mksub "別グループへ"
                        (mapcar (lambda (g) (mkmi (second g) (lambda () (move-to-group gk id (first g)))))
                                (remove gk (groups) :key #'first))))))
         (sep-ctx (gk id)               ; 区切り行の右クリック (上へ/下へ/削除)
           (ctx (list
                 (mkmi "上へ" (lambda () (move-cmd gk id -1)))
                 (mkmi "下へ" (lambda () (move-cmd gk id 1)))
                 (mkmi "削除" (lambda () (remove-cmd gk id))))))
         (grp-ctx (gk)
           (let ((cur (third (find gk (groups) :key #'first))))
             (ctx (list
                   (mksub "コマンドを追加"
                          (mapcar (lambda (c) (mkmi (command-label c) (lambda () (add-cmd gk (command-id c)))))
                                  (remove-if (lambda (c) (member (command-id c) cur)) *commands*)))
                   (mkmi "区切り線を追加" (lambda () (add-sep gk)))
                   (mkmi "グループを上へ" (lambda () (move-group gk -1)))
                   (mkmi "グループを下へ" (lambda () (move-group gk 1)))
                   (mkmi "グループ名を変更…" (lambda () (rename-group gk)))
                   (mkmi "グループを削除" (lambda () (delete-group gk)))))))
         (refresh ()
           (let ((items (dotnet:invoke tree "get_Items")))
             (dotnet:invoke items "Clear")
             (dolist (g (groups))
               (let ((gi (dotnet:new "Avalonia.Controls.TreeViewItem"))
                     (gk (first g)))
                 (dotnet:invoke gi "set_Header" (format nil "~a (~d)" (second g) (length (third g))))
                 (dotnet:invoke gi "set_IsExpanded" t)
                 (dotnet:invoke gi "set_ContextMenu" (grp-ctx gk))
                 (setup-drop gi (let ((k gk)) (lambda () (drop-on-grp k))))  ; グループ末尾へ
                 (let ((gci (dotnet:invoke gi "get_Items")))
                   (dolist (id (third g))
                     (let ((ci (dotnet:new "Avalonia.Controls.TreeViewItem")) (sep (%menu-sep-p id)))
                       (dotnet:invoke ci "set_Header"
                                      (if sep "──── 区切り ────"
                                          (let ((c (%command id)))
                                            (if c (command-label c) (format nil "~a" id)))))
                       (dotnet:invoke ci "set_ContextMenu" (if sep (sep-ctx gk id) (cmd-ctx gk id)))
                       (setup-drag ci gk id)
                       (setup-drop ci (let ((k gk) (i id)) (lambda () (drop-on-cmd k i))))
                       (dotnet:invoke gci "Add" ci))))
                 (dotnet:invoke items "Add" gi))))))
      (refresh)
      (dotnet:invoke tree "set_MaxHeight" 440d0)
      (dotnet:invoke tree "set_MinWidth" 340d0)
      (let* ((root (dotnet:new "Avalonia.Controls.StackPanel"))
             (hint (dotnet:new "Avalonia.Controls.TextBlock"))
             (btns (dotnet:new "Avalonia.Controls.StackPanel"))
             (addgrp-btn (%s-button "グループ追加" (lambda () (add-group))))
             (reset-btn (%s-button "既定に戻す"
                          (lambda () (setf (car mbox) (%menu-norm *default-command-menu*)) (refresh))))
             (cancel-btn (%s-button "キャンセル" (lambda () (dotnet:invoke w "Close"))))
             (ok-btn (%s-button "OK"
                       (lambda ()
                         (setf *command-menu* (%menu-denorm (groups)))
                         (save-menu) (%rebuild-menus vw) (dotnet:invoke w "Close")))))
        (dotnet:invoke hint "set_Text" "ドラッグで並べ替え (コマンド上=直前へ / グループ見出し=末尾へ)。右クリック → コマンド: 上へ/下へ/削除/別グループへ、グループ: コマンド・区切り追加/グループ上下/名前変更/削除")
        (dotnet:invoke hint "set_TextWrapping" (dotnet:static "Avalonia.Media.TextWrapping" "Wrap"))
        (dotnet:invoke hint "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 0d0 0d0 8d0))
        (dotnet:invoke btns "set_Orientation" (dotnet:static "Avalonia.Layout.Orientation" "Horizontal"))
        (dotnet:invoke btns "set_Margin" (dotnet:new "Avalonia.Thickness" 0d0 12d0 0d0 0d0))
        (let ((c (dotnet:invoke btns "get_Children")))
          (dotnet:invoke c "Add" addgrp-btn) (dotnet:invoke c "Add" reset-btn)
          (dotnet:invoke c "Add" cancel-btn) (dotnet:invoke c "Add" ok-btn))
        (dotnet:invoke root "set_Margin" (dotnet:new "Avalonia.Thickness" 12d0))
        (let ((c (dotnet:invoke root "get_Children")))
          (dotnet:invoke c "Add" hint) (dotnet:invoke c "Add" tree) (dotnet:invoke c "Add" btns))
        (dotnet:invoke w "set_Content" root))
      (dotnet:invoke w "set_Title" "メニュー編集")
      (dotnet:invoke w "set_Width" 400d0)
      (dotnet:invoke w "set_SizeToContent" (dotnet:static "Avalonia.Controls.SizeToContent" "Height"))
      (dotnet:invoke w "set_WindowStartupLocation" (dotnet:static "Avalonia.Controls.WindowStartupLocation" "CenterOwner"))
      (dotnet:invoke w "ShowDialog" owner))))

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
        (dotnet:invoke ebtn "set_Content" "掃き出し (zip)")
        (dotnet:static "Avalonia.Controls.DockPanel" "SetDock" ebtn
                       (dotnet:static "Avalonia.Controls.Dock" "Top"))
        (dotnet:add-event ebtn "Click" (lambda (s e) (declare (ignore s e)) (%pane-export vw)))
        (dotnet:invoke scr "set_Content" stack)
        (let ((pc (dotnet:invoke pane "get_Children")))
          (dotnet:invoke pc "Add" ebtn)
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
            (open-loader path
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
