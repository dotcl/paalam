;;;; util.lisp — paalam (#39 main.lisp 分割): ファイル名/画像ユーティリティ
(in-package :paalam)

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

