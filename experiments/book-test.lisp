;;;; book-test.lisp — book 抽象 (open-book / sort / load-image) の headless 検証
;;;; 実行: PAALAM_ZIP=C:/path/test.zip dotnet tool run dotcl experiments/book-test.lisp

(load (merge-pathnames "../src/main.lisp" *load-pathname*))
(in-package :paalam)

(ensure-wpf)  ; BitmapImage デコードに WPF が要る

(defun show-book (label path)
  (format t "~%== ~a: ~a ==~%" label path)
  (multiple-value-bind (book start) (open-book path)
    (let ((n (book-page-count book)))
      (format t "pages=~a start=~a~%" n start)
      (dotimes (i (min n 12))
        (let* ((bmp (book-load-image book i))
               (w (dotnet:invoke bmp "get_PixelWidth"))
               (h (dotnet:invoke bmp "get_PixelHeight")))
          (format t "  [~2d] ~a  (~ax~a)~%" i (book-page-name book i) w h)))
      (book-close book))))

(let ((zip (dotnet:static "System.Environment" "GetEnvironmentVariable" "PAALAM_ZIP"))
      (dir (dotnet:static "System.Environment" "GetEnvironmentVariable" "PAALAM_DIR")))
  (when (and zip (plusp (length zip))) (show-book "ZIP" zip))
  (when (and dir (plusp (length dir))) (show-book "DIR" dir))
  (unless (or zip dir)
    (show-book "DEFAULT (image file → parent folder)" *default-image*)))

(format t "~%book-test done.~%")
