;;;; pdf-ffi.lisp — paalam: PDF text/metadata/outline via direct pdfium FFI.
;;;;
;;;; Formerly the C# shim Paalam.PdfText (raw [DllImport] P/Invoke). dotcl can't emit
;;;; static P/Invoke stubs, but it CAN call native functions dynamically via dotnet:ffi
;;;; (NativeLibrary + calli). The byte[] is pinned with GCHandle and its
;;;; AddrOfPinnedObject is passed as a :ptr — the same pattern the C# shim used.
;;;;
;;;; pdfium is rendering-only via PDFtoImage (kept in C# Paalam.PdfDoc, SkiaSharp interop);
;;;; this file covers only the text-layer / metadata / bookmark APIs (#23 / #36).
(in-package :paalam)

;;; --- library resolution ---------------------------------------------------
;;; Unlike DllImport, dotnet:ffi does NOT do the .NET native-library probing: no lib/.so/.dll
;;; prefix/suffix guessing AND no runtimes/<rid>/native/ lookup. So we resolve the path
;;; ourselves, arch-agnostically (any RID, not just the big three OSes):
;;;   1. <base>/<libfile>                      — self-contained publish flattens it next to apphost
;;;   2. <base>/runtimes/<rid>/native/<libfile> — framework-dependent / `dotnet run`
;;;   3. <base>/<rid>/<libfile>                 — `dotnet build` RID-subdir output
;;;   4. <libfile>                              — last resort: OS loader search path
;;; <rid> comes from RuntimeInformation.RuntimeIdentifier, so a new arch needs no code change
;;; as long as PDFtoImage ships that RID's pdfium.
(defun %pdfium-libfile ()
  (cond ((dotnet:static "System.OperatingSystem" "IsWindows") "pdfium.dll")
        ((dotnet:static "System.OperatingSystem" "IsMacOS")   "libpdfium.dylib")
        (t                                                    "libpdfium.so")))

(defun %path-combine (&rest parts)
  "System.IO.Path.Combine over PARTS (each a string)."
  (reduce (lambda (a b) (dotnet:static "System.IO.Path" "Combine" a b)) parts))

(defun %file-exists (p) (dotnet:static "System.IO.File" "Exists" p))

(defun %resolve-pdfium ()
  "Pick the first existing pdfium path among the candidates; fall back to the bare filename
so the OS loader can still try its search path. Result is what dotnet:ffi gets as the lib."
  (let* ((base (dotnet:static "System.AppContext" "BaseDirectory"))
         (rid  (ignore-errors
                 (dotnet:static "System.Runtime.InteropServices.RuntimeInformation"
                                "get_RuntimeIdentifier")))
         (lib  (%pdfium-libfile))
         (cands (remove nil
                  (list (%path-combine base lib)
                        (when rid (%path-combine base "runtimes" rid "native" lib))
                        (when rid (%path-combine base rid lib))))))
    (or (find-if #'%file-exists cands) lib)))

(defvar *pdfium-lib* nil
  "Resolved pdfium library path for dotnet:ffi (filled lazily by %pdfium-lib).")

(defun %pdfium-lib ()
  (or *pdfium-lib* (setf *pdfium-lib* (%resolve-pdfium))))

(defun %pf (fn &rest opts)
  "Call pdfium function FN. OPTS is (:args '(types...) :ret type value...)."
  (apply #'dotnet:ffi (%pdfium-lib) fn opts))

(defun %pf-bool (fn &rest opts)
  "Like %pf for a :ret :bool call. dotnet:ffi returns 0/1 (an integer); in CL 0 is still
true, so coerce to a real Lisp boolean. OPTS is the usual (:args ... :ret :bool value...)."
  (plusp (apply #'dotnet:ffi (%pdfium-lib) fn opts)))

;;; --- pinning helpers ------------------------------------------------------
;;; FFI passes raw addresses, so a managed array handed to native code must be pinned
;;; for the duration of the call(s). %with-pinned binds VAR to the pinned int64 address
;;; and frees the GCHandle on unwind.
(defun %gc-pin (arr)
  (dotnet:static "System.Runtime.InteropServices.GCHandle" "Alloc"
                 arr (dotnet:static "System.Runtime.InteropServices.GCHandleType" "Pinned")))
(defun %gc-addr (gch)
  (dotnet:invoke (dotnet:invoke gch "AddrOfPinnedObject") "ToInt64"))

(defmacro %with-pinned ((addr-var arr) &body body)
  "Pin ARR; bind ADDR-VAR to its pinned int64 address; free on unwind."
  (let ((g (gensym "GCH")))
    `(let ((,g (%gc-pin ,arr)))
       (unwind-protect
            (let ((,addr-var (%gc-addr ,g))) ,@body)
         (dotnet:invoke ,g "Free")))))

;;; --- typed array construction ---------------------------------------------
(defun %net-array (type-name n)
  (dotnet:static "System.Array" "CreateInstance"
                 (dotnet:static "System.Type" "GetType" type-name) n))

(defun %u16 (n) (dotnet:static "System.Convert" "ToUInt16" n))

(defun %utf16z (s)
  "FPDF_WIDESTRING: a UInt16[] of UTF-16 code units, NUL-terminated."
  (let ((a (%net-array "System.UInt16" (1+ (length s)))))
    (loop for ch across s for i from 0
          do (dotnet:invoke a "SetValue" (%u16 (char-code ch)) i))
    (dotnet:invoke a "SetValue" (%u16 0) (length s))
    a))

(defun %utf16-string (byte-array nbytes cap)
  "Decode a pdfium UTF-16LE out-buffer (BYTE-ARRAY, NBYTES written) to a Lisp string,
trimming the trailing NUL."
  (let* ((enc (dotnet:static "System.Text.Encoding" "get_Unicode"))
         (str (dotnet:invoke enc "GetString" byte-array 0 (min nbytes cap))))
    (string-trim '(#\Nul) str)))

;;; --- pdfium lifecycle -----------------------------------------------------
(defvar *pdfium-inited* nil)
(defun %pdfium-init ()
  (unless *pdfium-inited*
    (%pf "FPDF_InitLibrary" :args 'nil :ret :void)
    (setf *pdfium-inited* t)))

(defmacro %with-doc ((doc-var bytes) &body body)
  "Pin BYTES, FPDF_LoadMemDocument into DOC-VAR, run BODY, always close the doc.
BODY is skipped (returns the empty-form value via the surrounding code) if load fails;
callers check (zerop DOC-VAR) themselves where they want a specific empty value."
  (let ((g (gensym "GCH")) (len (gensym "LEN")))
    `(progn
       (%pdfium-init)
       (let* ((,len (dotnet:invoke ,bytes "get_Length"))
              (,g   (%gc-pin ,bytes)))
         (unwind-protect
              (let ((,doc-var (%pf "FPDF_LoadMemDocument" :args '(:ptr :int :ptr) :ret :ptr
                                   (%gc-addr ,g) ,len 0)))
                (unwind-protect
                     (progn ,@body)
                  (unless (zerop ,doc-var)
                    (%pf "FPDF_CloseDocument" :args '(:ptr) :ret :void ,doc-var))))
           (dotnet:invoke ,g "Free"))))))

;;; --- public API (mirrors the old Paalam.PdfText) --------------------------

(defun %pdf-search-pages (bytes query)
  "0-based list of page indices containing QUERY (case-insensitive, in order). #23."
  (if (or (null bytes) (zerop (length query)))
      '()
      (%with-doc (doc bytes)
        (if (zerop doc)
            '()
            (%with-pinned (fw-addr (%utf16z query))
              (let ((n (%pf "FPDF_GetPageCount" :args '(:ptr) :ret :int doc))
                    (hits '()))
                (dotimes (i n (nreverse hits))
                  (let ((page (%pf "FPDF_LoadPage" :args '(:ptr :int) :ret :ptr doc i)))
                    (unless (zerop page)
                      (let ((tp (%pf "FPDFText_LoadPage" :args '(:ptr) :ret :ptr page)))
                        (unless (zerop tp)
                          (let ((sh (%pf "FPDFText_FindStart" :args '(:ptr :ptr :uint32 :int) :ret :ptr
                                         tp fw-addr 0 0)))
                            (unless (zerop sh)
                              (when (%pf-bool "FPDFText_FindNext" :args '(:ptr) :ret :bool sh)
                                (push i hits))
                              (%pf "FPDFText_FindClose" :args '(:ptr) :ret :void sh)))
                          (%pf "FPDFText_ClosePage" :args '(:ptr) :ret :void tp)))
                      (%pf "FPDF_ClosePage" :args '(:ptr) :ret :void page))))))))))

(defun %pdf-match-rects (bytes page-index query)
  "Normalized [x y w h x y w h ...] rects (0..1, top-left origin) of QUERY's first hit on
PAGE-INDEX. Empty list if none. The caller multiplies by display dimensions. #23."
  (if (or (null bytes) (zerop (length query)))
      '()
      (%with-doc (doc bytes)
        (if (zerop doc)
            '()
            (let ((page (%pf "FPDF_LoadPage" :args '(:ptr :int) :ret :ptr doc page-index)))
              (if (zerop page)
                  '()
                  (unwind-protect
                       (let ((tp (%pf "FPDFText_LoadPage" :args '(:ptr) :ret :ptr page)))
                         (if (zerop tp)
                             '()
                             (unwind-protect
                                  (%with-pinned (fw-addr (%utf16z query))
                                    (let ((sh (%pf "FPDFText_FindStart" :args '(:ptr :ptr :uint32 :int) :ret :ptr
                                                   tp fw-addr 0 0)))
                                      (if (or (zerop sh)
                                              (not (%pf-bool "FPDFText_FindNext" :args '(:ptr) :ret :bool sh)))
                                          (progn (unless (zerop sh)
                                                   (%pf "FPDFText_FindClose" :args '(:ptr) :ret :void sh))
                                                 '())
                                          (unwind-protect
                                               (%pdf-rects-of-hit tp page sh)
                                            (%pf "FPDFText_FindClose" :args '(:ptr) :ret :void sh)))))
                               (%pf "FPDFText_ClosePage" :args '(:ptr) :ret :void tp))))
                    (%pf "FPDF_ClosePage" :args '(:ptr) :ret :void page))))))))

(defun %net-double-ref (arr i) (dotnet:invoke arr "GetValue" i))

(defun %pdf-rects-of-hit (tp page sh)
  "Collect FPDFText_CountRects rects for the current search hit, normalized by page size.
PDF origin is bottom-left; y is flipped so the result uses a top-left origin."
  (let* ((start (%pf "FPDFText_GetSchResultIndex" :args '(:ptr) :ret :int sh))
         (cnt   (%pf "FPDFText_GetSchCount" :args '(:ptr) :ret :int sh))
         (nr    (%pf "FPDFText_CountRects" :args '(:ptr :int :int) :ret :int tp start cnt))
         (pw    (%pf "FPDF_GetPageWidthF"  :args '(:ptr) :ret :float page))
         (ph    (%pf "FPDF_GetPageHeightF" :args '(:ptr) :ret :float page))
         (arr   (%net-array "System.Double" 4))   ; out slots l,t,r,b — pinned once, reused per rect
         (out   '()))
    (if (or (<= pw 0) (<= ph 0))
        '()
        (progn
          (%with-pinned (base arr)
            ;; out double l,t,r,b — pass &slot (base + i*8) as :ptr; read back via GetValue.
            (dotimes (i nr)
              (%pf "FPDFText_GetRect" :args '(:ptr :int :ptr :ptr :ptr :ptr) :ret :void
                   tp i base (+ base 8) (+ base 16) (+ base 24))
              (let ((l (%net-double-ref arr 0)) (tt (%net-double-ref arr 1))
                    (r (%net-double-ref arr 2)) (b (%net-double-ref arr 3)))
                (push (/ l pw) out)               ; x
                (push (/ (- ph tt) ph) out)       ; y (flip)
                (push (/ (- r l) pw) out)         ; w
                (push (/ (- tt b) ph) out))))     ; h
          (nreverse out)))))

(defun %pdf-meta (bytes tag)
  "Document metadata string for TAG (\"Title\"/\"Author\"/... ). \"\" if absent. #36."
  (if (or (null bytes) (zerop (length tag)))
      ""
      (%with-doc (doc bytes)
        (if (zerop doc)
            ""
            (let* ((cap 8192)
                   (buf (%net-array "System.Byte" cap)))
              (%with-pinned (addr buf)
                (let ((n (%pf "FPDF_GetMetaText" :args '(:ptr :string :ptr :uint32) :ret :uint32
                              doc tag addr cap)))
                  (if (<= n 2) "" (%utf16-string buf n cap)))))))))

(defun %pdf-outline (bytes)
  "PDF bookmarks as a string of \"depth<TAB>page<TAB>title\" lines (depth-first). \"\" if none.
Same delimited format the caller (%parse-outline) already expects. #23."
  (if (null bytes)
      ""
      (%with-doc (doc bytes)
        (if (zerop doc)
            ""
            (let ((sb (dotnet:new "System.Text.StringBuilder"))
                  (cap 2048))
              (labels ((walk (bm depth)
                         (let ((guard 0))
                           (loop while (and (not (zerop bm)) (< guard 5000))
                                 do (incf guard)
                                    (let* ((buf (%net-array "System.Byte" cap))
                                           (title
                                             (%with-pinned (addr buf)
                                               (let ((n (%pf "FPDFBookmark_GetTitle" :args '(:ptr :ptr :uint32) :ret :uint32
                                                             bm addr cap)))
                                                 (if (> n 2) (%utf16-string buf n cap) ""))))
                                           (dest (%pf "FPDFBookmark_GetDest" :args '(:ptr :ptr) :ret :ptr doc bm))
                                           (page (if (zerop dest) -1
                                                     (%pf "FPDFDest_GetDestPageIndex" :args '(:ptr :ptr) :ret :int doc dest))))
                                      (dotnet:invoke sb "Append"
                                                     (format nil "~d~a~d~a~a~%"
                                                             depth #\Tab page #\Tab
                                                             (%clean-line title)))
                                      (walk (%pf "FPDFBookmark_GetFirstChild" :args '(:ptr :ptr) :ret :ptr doc bm)
                                            (1+ depth))
                                      (setf bm (%pf "FPDFBookmark_GetNextSibling" :args '(:ptr :ptr) :ret :ptr doc bm)))))))
                (walk (%pf "FPDFBookmark_GetFirstChild" :args '(:ptr :ptr) :ret :ptr doc 0) 0))
              (dotnet:invoke sb "ToString"))))))

(defun %clean-line (s)
  "Replace tab/CR/LF in a title with spaces (so it survives the line/TAB delimited format)."
  (map 'string (lambda (c) (if (member c '(#\Tab #\Newline #\Return)) #\Space c)) s))
