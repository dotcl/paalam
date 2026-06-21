;;;; util.lisp — paalam (#39 main.lisp split): filename/image utilities
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; Filename utilities
;;; ------------------------------------------------------------------

(defparameter *image-extensions* '("jpg" "jpeg" "png" "gif" "bmp" "tif" "tiff" "webp")
  "Supported image extensions (lowercase). Avalonia/Skia can also read webp.")

(defun image-ext-p (ext)
  (and ext (member (string-downcase ext) *image-extensions* :test #'string=) t))

(defun image-name-p (name)
  (let ((dot (position #\. name :from-end t)))
    (and dot (image-ext-p (subseq name (1+ dot))))))

(defun natural-key (s)
  "Natural-sort key. Zero-pads digit runs and lowercases letters (page2 < page10)."
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
;;; Image loading (Avalonia.Media.Imaging.Bitmap)
;;; ------------------------------------------------------------------

(defun %load-file-bitmap (path)
  (dotnet:new "Avalonia.Media.Imaging.Bitmap" path))   ; Bitmap(string fileName)

(defun %load-zip-bitmap (entry)
  ;; ZipArchiveEntry.Open() is non-seekable, so copy it into a MemoryStream first.
  ;; Avalonia Bitmap(Stream) decodes inside the ctor, so the stream may be closed afterward.
  (let ((es (dotnet:invoke entry "Open"))
        (ms (dotnet:new "System.IO.MemoryStream")))
    (unwind-protect
         (progn (dotnet:invoke es "CopyTo" ms)
                (dotnet:invoke ms "set_Position" 0)
                ;; #294 fixed (DotCL.Runtime 0.1.12): dotnet:new selects Bitmap(Stream)
                ;; correctly, so no C# shim is needed (direct call).
                (dotnet:new "Avalonia.Media.Imaging.Bitmap" ms))
      (dotnet:invoke es "Dispose")
      (dotnet:invoke ms "Dispose"))))

