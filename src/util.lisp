;;;; util.lisp — paalam (#39 main.lisp split): filename/image utilities
(in-package :paalam)

;;; ------------------------------------------------------------------
;;; Filename utilities
;;; ------------------------------------------------------------------

(defparameter *image-extensions* '("jpg" "jpeg" "png" "gif" "bmp" "tif" "tiff" "webp" "heic" "heif")
  "Supported image extensions (lowercase). Avalonia/Skia read most natively; heic/heif go
through the SkiaSharp.Heic decoder (#13, see %heic->bitmap).")

(defun image-ext-p (ext)
  (and ext (member (string-downcase ext) *image-extensions* :test #'string=) t))

(defun heic-ext-p (ext)
  "Whether EXT needs the HEIC decoder (Avalonia's Bitmap can't read these natively)."
  (and ext (member (string-downcase ext) '("heic" "heif") :test #'string=) t))

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

;;; HEIC/HEIF decode (#13). Avalonia's Bitmap (SkiaSharp SKCodec) can't read HEIF, so route it
;;; through SkiaSharp.Heic (pure C#, no native deps): decode to an SKBitmap, re-encode to PNG,
;;; and hand that to Avalonia's Bitmap(Stream). Done directly in Lisp (no C# shim), reusing the
;;; SkiaSharp that PDFtoImage already pulls in.
(defun %heic->bitmap (bytes)
  "Decode HEIC/HEIF BYTES to an Avalonia Bitmap, or NIL on failure."
  (handler-case
      (let ((skb (dotnet:static "SkiaSharp.Heic.HeicDecoder" "Decode" bytes)))
        (when skb
          (let* ((img  (dotnet:static "SkiaSharp.SKImage" "FromBitmap" skb))
                 (data (dotnet:invoke img "Encode"
                                      (dotnet:static "SkiaSharp.SKEncodedImageFormat" "Png") 100))
                 (png  (dotnet:invoke data "ToArray"))
                 (ms   (dotnet:new "System.IO.MemoryStream" png)))
            (unwind-protect
                 (dotnet:new "Avalonia.Media.Imaging.Bitmap" ms)
              (dotnet:invoke ms "Dispose")
              (ignore-errors (dotnet:invoke data "Dispose"))
              (ignore-errors (dotnet:invoke img "Dispose"))
              (ignore-errors (dotnet:invoke skb "Dispose"))))))
    (error (e) (log-err "heic-decode" e) nil)))

(defun %bytes->bitmap (bytes ext)
  "Decode image BYTES to an Avalonia Bitmap. EXT picks the path: heic/heif via %heic->bitmap,
everything else via Avalonia's own Bitmap(Stream)."
  (if (heic-ext-p ext)
      (%heic->bitmap bytes)
      (let ((ms (dotnet:new "System.IO.MemoryStream" bytes)))
        (unwind-protect (dotnet:new "Avalonia.Media.Imaging.Bitmap" ms)
          (dotnet:invoke ms "Dispose")))))

(defun %load-file-bitmap (path)
  ;; heic/heif need the decoder (read bytes); other formats use Avalonia's fast path-ctor.
  (if (heic-ext-p (pathname-type path))
      (%heic->bitmap (dotnet:static "System.IO.File" "ReadAllBytes" path))
      (dotnet:new "Avalonia.Media.Imaging.Bitmap" path)))   ; Bitmap(string fileName)

(defun %load-zip-bitmap (entry &optional name)
  ;; ZipArchiveEntry.Open() is non-seekable, so copy it into a MemoryStream first.
  ;; Avalonia Bitmap(Stream) decodes inside the ctor, so the stream may be closed afterward.
  ;; NAME (the entry's filename) selects the heic path; nil keeps the native fast path.
  (let ((es (dotnet:invoke entry "Open"))
        (ms (dotnet:new "System.IO.MemoryStream")))
    (unwind-protect
         (progn (dotnet:invoke es "CopyTo" ms)
                (if (and name (heic-ext-p (pathname-type name)))
                    (%heic->bitmap (dotnet:invoke ms "ToArray"))
                    (progn
                      (dotnet:invoke ms "set_Position" 0)
                      ;; #294 fixed (DotCL.Runtime 0.1.12): dotnet:new selects Bitmap(Stream)
                      ;; correctly, so no C# shim is needed (direct call).
                      (dotnet:new "Avalonia.Media.Imaging.Bitmap" ms))))
      (dotnet:invoke es "Dispose")
      (dotnet:invoke ms "Dispose"))))

