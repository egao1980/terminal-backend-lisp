(in-package #:terminal-backend-lisp)

;;; Kitty graphics protocol, raw pixels only (feature :KITTY-GRAPHICS).
;;;
;;;   ESC _ G <key=value,…> [; <base64 payload>] ST
;;;
;;; Bad input from the program is not a Lisp error for the caller of
;;; TERMINAL-FEED: validation signals KITTY-REFUSAL, %APC-DISPATCH catches it
;;; by type and turns it into an error reply (or silence under q=2). The reply
;;; is sent after the handler has unwound, so host errors keep the protocol's
;;; own path. Validation runs before any mutation of the store, and the
;;; mutation helpers cannot signal, so a refusal leaves the store unchanged.
;;;
;;; Supported: a=t T p q d; f=24 (:RGB) and f=32 (:RGBA); t=d (direct) only;
;;; chunking with m. Compression (o=z), PNG (f=100) and file media are refused
;;; with EINVAL; files are never read. Extra payload bytes beyond s·v·bpp are
;;; ignored. Placements stay at the screen row they were made on.

(defconstant +kitty-max-value+ #xFFFFFFFF)
(defconstant +kitty-max-upload+ (* 64 1024 1024)) ; base64 characters across chunks
(defconstant +kitty-below-background-z+ -1073741824)

(define-condition kitty-refusal (error)
  ((code :initarg :code :reader kitty-refusal-code)       ; "EINVAL" | "ENOENT"
   (message :initarg :message :reader kitty-refusal-message))
  (:report (lambda (c s)
             (format s "Kitty graphics command refused: ~A: ~A"
                     (kitty-refusal-code c) (kitty-refusal-message c))))
  (:documentation "Internal. Signalled by graphics validation for bad input
from the program. Caught only by %APC-DISPATCH, which answers the program
with an error reply. It never leaves TERMINAL-FEED."))

(defun %refuse (code message)
  (error 'kitty-refusal :code code :message message))

;;; ------------------------------------------------------------------
;;; Control keys

(defun %kitty-fields (control)
  "\"a=T,i=7\" → ((#\\a . \"T\") (#\\i . \"7\")), in order. Fields that are not
a single-character key followed by = are skipped. Never signals."
  (let ((fields '()) (start 0) (n (length control)))
    (loop
      (let ((end (or (position #\, control :start start) n)))
        (when (and (>= (- end start) 2) (char= (char control (1+ start)) #\=))
          (push (cons (char control start) (subseq control (+ start 2) end)) fields))
        (when (>= end n) (return (nreverse fields)))
        (setf start (1+ end))))))

(defun %lenient-key (fields key &optional default)
  "The integer value of KEY in FIELDS, or DEFAULT when it is absent or not a
number. Used for reply targets and chunk flags, which must survive a refusal."
  (let ((entry (assoc key fields)))
    (or (and entry (parse-integer (cdr entry) :junk-allowed t)) default)))

(defun %kitty-kind (key)
  "→ :CHAR, :INT, :UINT, or NIL for a key this backend ignores."
  (cond ((find key "adot") :char)
        ((char= key #\z) :int)
        ((find key "fqmiIpsvxywhcrXYC") :uint)
        (t nil)))

(defun %kitty-value (string kind)
  (ecase kind
    (:char (if (= (length string) 1)
               (char string 0)
               (%refuse "EINVAL" "invalid data")))
    ((:uint :int)
     (let* ((negative (and (eq kind :int) (plusp (length string))
                           (char= (char string 0) #\-)))
            (digits (if negative (subseq string 1) string)))
       (unless (and (<= 1 (length digits) 10) (every #'digit-char-p digits))
         (%refuse "EINVAL" "invalid data"))
       (let ((n (parse-integer digits)))
         (when (> n +kitty-max-value+) (%refuse "EINVAL" "invalid data"))
         (if negative (- n) n))))))

(defun %kitty-command (fields)
  "FIELDS → the typed command, an alist keyed by character (case-sensitive).
Unknown keys are dropped. Signals KITTY-REFUSAL for a malformed value."
  (loop for (key . string) in fields
        for kind = (%kitty-kind key)
        when kind collect (cons key (%kitty-value string kind))))

(declaim (inline %kc))
(defun %kc (command key &optional default)
  (let ((entry (assoc key command)))
    (if entry (cdr entry) default)))

;;; ------------------------------------------------------------------
;;; Images

(defun %decode-image (term command base64)
  "Validate COMMAND and its BASE64 payload → a fresh TP:TERMINAL-IMAGE.
Mutates nothing. Signals KITTY-REFUSAL."
  (let* ((f (%kc command #\f 32))
         (bpp (case f (24 3) (32 4)))
         (w (%kc command #\s 0))
         (h (%kc command #\v 0)))
    (cond ((%kc command #\o) (%refuse "EINVAL" "unsupported compression"))
          ((not (eql (%kc command #\t #\d) #\d)) (%refuse "EINVAL" "unsupported medium"))
          ((null bpp) (%refuse "EINVAL" "unsupported format"))
          ((or (eq base64 :overflow) (zerop w) (zerop h)) (%refuse "EINVAL" "invalid data")))
    (let ((octets (or (%base64-decode base64) (%refuse "EINVAL" "invalid data")))
          (need (* w h bpp)))
      (when (< (length octets) need) (%refuse "EINVAL" "invalid data"))
      (tp:make-terminal-image :id (%kitty-image-id term command)
                              :width w :height h
                              :format (if (= bpp 3) :rgb :rgba)
                              :pixels (if (= (length octets) need)
                                          octets
                                          (subseq octets 0 need))))))

(defun %kitty-image-id (term command)
  "The i key, or the next free automatic id (not consumed until stored)."
  (let ((i (%kc command #\i 0)))
    (if (plusp i) i (%ks-next-id (%kitty term)))))

(defun %store-image (term image)
  "Store IMAGE. Re-transmitting an id replaces the image and drops its placements."
  (let ((store (%kitty term))
        (id (tp:terminal-image-id image)))
    (when (= id (%ks-next-id store)) (incf (%ks-next-id store)))
    (%drop-placements term (lambda (pl) (= (tp:image-placement-image-id pl) id)))
    (setf (gethash id (%ks-images store)) image)))

;;; ------------------------------------------------------------------
;;; Placements

(defun %ceil-div (n d)
  "⌈N/D⌉, at least 1; 1 when D (a cell size) is unknown."
  (if (plusp d) (max 1 (ceiling n d)) 1))

(defun %kitty-layer (z)
  (cond ((< z +kitty-below-background-z+) :below-background)
        ((minusp z) :below-text)
        (t :above-text)))

(defun %placement-geometry (command image x y cw ch)
  "A fresh TP:IMAGE-PLACEMENT of IMAGE at cell (X, Y), cell size CW × CH
pixels (0 when unknown). Pure; signals KITTY-REFUSAL for an empty source."
  (let* ((iw (tp:terminal-image-width image))
         (ih (tp:terminal-image-height image))
         (sx (%kc command #\x 0))
         (sy (%kc command #\y 0)))
    (unless (and (< sx iw) (< sy ih)) (%refuse "EINVAL" "invalid data"))
    (let* ((sw (min (let ((w (%kc command #\w 0))) (if (plusp w) w (- iw sx))) (- iw sx)))
           (sh (min (let ((h (%kc command #\h 0))) (if (plusp h) h (- ih sy))) (- ih sy)))
           (c (let ((c (%kc command #\c 0))) (and (plusp c) c)))
           (r (let ((r (%kc command #\r 0))) (and (plusp r) r)))
           (ox (%kc command #\X 0))
           (oy (%kc command #\Y 0))
           (z (%kc command #\z 0)))
      ;; Only one of c / r: derive the other from the source aspect ratio.
      (cond ((and c (not r)) (setf r (%ceil-div (* c cw sh) (* sw ch))))
            ((and r (not c)) (setf c (%ceil-div (* r ch sw) (* sh cw)))))
      (multiple-value-bind (cols rows pw ph)
          (if c
              (values c r (* c cw) (* r ch))
              (values (%ceil-div (+ ox sw) cw) (%ceil-div (+ oy sh) ch) sw sh))
        (tp:make-image-placement
         :image-id (tp:terminal-image-id image)
         :placement-id (%kc command #\p 0)
         :x x :y y :cols cols :rows rows :z z :layer (%kitty-layer z)
         :cell-offset-x ox :cell-offset-y oy :pixel-width pw :pixel-height ph
         :source-x sx :source-y sy :source-width sw :source-height sh)))))

(defun %drop-placements (term test)
  "Remove the placements satisfying TEST. Bumps the epoch when any go."
  (let* ((placements (%ks-placements (%kitty term)))
         (before (length placements))
         (kept (remove-if test placements :key #'cdr)))
    (unless (= before (length kept))
      (setf (fill-pointer placements) 0)
      (loop for entry across kept do (vector-push-extend entry placements))
      (%bump-epoch term))))

(defun %place-image (term command image)
  "Place IMAGE at the cursor, then move the cursor unless C=1."
  (let* ((s (%screen term))
         (pl (%placement-geometry command image (screen-x s) (screen-y s)
                                  (%cell-width-px term) (%cell-height-px term)))
         (id (tp:image-placement-image-id pl))
         (pid (tp:image-placement-placement-id pl)))
    (when (plusp pid)                    ; the same (i, p≠0) replaces
      (%drop-placements term (lambda (old)
                               (and (= (tp:image-placement-image-id old) id)
                                    (= (tp:image-placement-placement-id old) pid)))))
    (vector-push-extend (cons (%active-name term) pl) (%ks-placements (%kitty term)))
    (%bump-epoch term)
    (unless (eql (%kc command #\C 0) 1)
      ;; Down rows−1 (scrolling at the bottom margin; at most a screen
      ;; height), then right by cols, clamped to the last column.
      (loop repeat (min (1- (tp:image-placement-rows pl)) (%rows term))
            do (%index term))
      (%move-to term (+ (screen-x s) (tp:image-placement-cols pl)) (screen-y s)))))

(defun %delete-images (term command)
  "a=d. d=a/A (or none): every placement; d=i/I: those of image i (only
placement p when given). Upper case also frees the image data. Other d
values and unknown ids are ignored."
  (let ((d (%kc command #\d #\a))
        (id (%kc command #\i 0))
        (pid (%kc command #\p 0))
        (images (%ks-images (%kitty term))))
    (case d
      ((#\a #\A)
       (%drop-placements term (constantly t))
       (when (char= d #\A) (clrhash images)))
      ((#\i #\I)
       (when (plusp id)
         (%drop-placements term (lambda (pl)
                                  (and (= (tp:image-placement-image-id pl) id)
                                       (or (zerop pid)
                                           (= (tp:image-placement-placement-id pl) pid)))))
         (when (char= d #\I) (remhash id images)))))))

;;; ------------------------------------------------------------------
;;; Commands

(defun %kitty-execute (term command payload)
  "Run one complete COMMAND. → T when an OK reply is due. Signals KITTY-REFUSAL."
  (case (%kc command #\a #\t)
    (#\t (%store-image term (%decode-image term command payload)) t)
    (#\T (let ((image (%decode-image term command payload)))
           ;; Geometry is validated before anything is stored.
           (%placement-geometry command image 0 0 0 0)
           (%store-image term image)
           (%place-image term command image))
         t)
    (#\q (%decode-image term command payload) t) ; validate only, store nothing
    (#\p (let ((image (gethash (%kc command #\i 0) (%ks-images (%kitty term)))))
           (unless image (%refuse "ENOENT" "image not found"))
           (%place-image term command image))
         t)
    (#\d (%delete-images term command) nil)
    (t nil)))

(defun %kitty-assemble (term fields payload overflow)
  "Collect chunks. → (values governing-fields payload) for a complete command,
or NIL while an upload continues. The first chunk's keys govern; later chunks
count only for m. PAYLOAD is :OVERFLOW when the data was too large."
  (let* ((store (%kitty term))
         (pending (%ks-pending store))
         (more (eql (%lenient-key fields #\m 0) 1)))
    (flet ((append-payload (entry)
             (let ((text (cdr entry)))
               (if (or overflow (eq text :overflow)
                       (> (+ (length text) (length payload)) +kitty-max-upload+))
                   (setf (cdr entry) :overflow)
                   (loop for ch across payload do (vector-push-extend ch text))))))
      (cond (pending
             (append-payload pending)
             (unless more
               (setf (%ks-pending store) nil)
               (values (car pending) (cdr pending))))
            ((and more (member (or (cdr (assoc #\a fields)) "t") '("t" "T" "q")
                               :test #'string=))
             (let ((entry (cons fields (make-array (length payload) :element-type 'character
                                                                    :adjustable t
                                                                    :fill-pointer 0))))
               (append-payload entry)
               (setf (%ks-pending store) entry)
               nil))
            (t (values fields (if overflow :overflow payload)))))))

(defun %kitty-reply-text (fields message errorp)
  "→ \"ESC _ G i=…[,I=…][,p=…];MESSAGE ESC \\\", or NIL when there is nobody to
answer (no i or I) or q suppresses it (1: OK replies, 2: all replies)."
  (let ((i (%lenient-key fields #\i 0))
        (number (%lenient-key fields #\I 0))
        (pid (%lenient-key fields #\p 0))
        (quiet (%lenient-key fields #\q 0)))
    (when (and (or (plusp i) (plusp number))
               (case quiet (0 t) (1 errorp) (t nil)))
      (with-output-to-string (out)
        (format out "~C_G" #\Esc)
        (format out "~{~A~^,~}"
                (remove nil (list (and (plusp i) (format nil "i=~D" i))
                                  (and (plusp number) (format nil "I=~D" number))
                                  (and (plusp pid) (format nil "p=~D" pid)))))
        (format out ";~A~C\\" message #\Esc)))))

(defun %apc-dispatch (term)
  "Run the APC collected in the parser. Only G (Kitty graphics) is understood;
any other APC is ignored."
  (let* ((p (%parser term))
         (buffer (parser-osc p)))
    (when (and (plusp (length buffer)) (= (aref buffer 0) (char-code #\G)))
      ;; Copy out first: nothing below reads the parser buffer.
      (let* ((text (map 'string #'code-char buffer))
             (semi (position #\; text))
             (fields (%kitty-fields (subseq text 1 (or semi (length text)))))
             (payload (if semi (subseq text (1+ semi)) "")))
        (multiple-value-bind (governing data)
            (%kitty-assemble term fields payload (parser-osc-overflow p))
          (when governing
            (let ((reply
                    (handler-case
                        (and (%kitty-execute term (%kitty-command governing) data)
                             (%kitty-reply-text governing "OK" nil))
                      (kitty-refusal (c)
                        (%kitty-reply-text governing
                                           (format nil "~A: ~A" (kitty-refusal-code c)
                                                   (kitty-refusal-message c))
                                           t)))))
              ;; Outside HANDLER-CASE: host errors take the protocol's path.
              (when reply (%reply term reply)))))))))
