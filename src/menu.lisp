;;;; menu.lisp — paalam (#39 main.lisp 分割): メニューバー (#21/#32)
(in-package :paalam)

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
