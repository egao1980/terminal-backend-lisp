(in-package #:terminal-backend-lisp)

;;; Byte codecs: UTF-8 and base64 in portable CL (no babel, no cl-base64).

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun %octets (list)
  "LIST of octets → a fresh OCTETS vector."
  (coerce list 'octets))

(defun %empty-octets ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun %utf8-push (code out)
  "Push the UTF-8 bytes of CODE onto the reversed list OUT. → the new list."
  (cond ((< code #x80) (push code out))
        ((< code #x800)
         (push (logior #xC0 (ash code -6)) out)
         (push (logior #x80 (logand code #x3F)) out))
        ((< code #x10000)
         (push (logior #xE0 (ash code -12)) out)
         (push (logior #x80 (logand (ash code -6) #x3F)) out)
         (push (logior #x80 (logand code #x3F)) out))
        (t
         (push (logior #xF0 (ash code -18)) out)
         (push (logior #x80 (logand (ash code -12) #x3F)) out)
         (push (logior #x80 (logand (ash code -6) #x3F)) out)
         (push (logior #x80 (logand code #x3F)) out)))
  out)

(defun %utf8-encode (string)
  "STRING → OCTETS."
  (let ((out '()))
    (loop for ch across string do (setf out (%utf8-push (char-code ch) out)))
    (%octets (nreverse out))))

(defun %ascii-octets (string)
  "An ASCII STRING → OCTETS."
  (map 'octets #'char-code string))

(defun %utf8-lead (byte)
  "→ (values continuation-count initial-bits minimum) for lead BYTE, or NIL."
  (cond ((<= #xC2 byte #xDF) (values 1 (logand byte #x1F) #x80))
        ((<= #xE0 byte #xEF) (values 2 (logand byte #x0F) #x800))
        ((<= #xF0 byte #xF4) (values 3 (logand byte #x07) #x10000))
        (t nil)))

(defun %valid-scalar-p (code minimum)
  (and (>= code minimum) (<= code #x10FFFF) (not (<= #xD800 code #xDFFF))))

(defun %utf8-decode (octets &key (start 0) (end (length octets)))
  "OCTETS[START, END) → string. Invalid sequences become U+FFFD."
  (with-output-to-string (out)
    (let ((i start))
      (loop while (< i end)
            do (let ((b (aref octets i)))
                 (incf i)
                 (if (< b #x80)
                     (write-char (code-char b) out)
                     (multiple-value-bind (need code minimum) (%utf8-lead b)
                       (if (null need)
                           (write-char (code-char #xFFFD) out)
                           (loop
                             (cond ((zerop need)
                                    (write-char (code-char (if (%valid-scalar-p code minimum)
                                                               code #xFFFD))
                                                out)
                                    (return))
                                   ((and (< i end) (<= #x80 (aref octets i) #xBF))
                                    (setf code (logior (ash code 6) (logand (aref octets i) #x3F)))
                                    (incf i)
                                    (decf need))
                                   (t (write-char (code-char #xFFFD) out)
                                      (return))))))))))))

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun %base64-encode (octets)
  "OCTETS → base64 string with padding."
  (with-output-to-string (out)
    (loop for i from 0 below (length octets) by 3
          do (let* ((n (min 3 (- (length octets) i)))
                    (b0 (aref octets i))
                    (b1 (if (> n 1) (aref octets (+ i 1)) 0))
                    (b2 (if (> n 2) (aref octets (+ i 2)) 0))
                    (word (logior (ash b0 16) (ash b1 8) b2)))
               (write-char (char +base64-alphabet+ (ldb (byte 6 18) word)) out)
               (write-char (char +base64-alphabet+ (ldb (byte 6 12) word)) out)
               (write-char (if (> n 1) (char +base64-alphabet+ (ldb (byte 6 6) word)) #\=) out)
               (write-char (if (> n 2) (char +base64-alphabet+ (ldb (byte 6 0) word)) #\=) out)))))

(defun %base64-decode (string)
  "Base64 STRING → OCTETS, or NIL if STRING is not valid base64. Padding is
optional; ASCII whitespace is skipped."
  (let ((out '()) (acc 0) (bits 0) (padding nil))
    (loop for ch across string
          do (cond ((member ch '(#\Space #\Tab #\Newline #\Return)))
                   ((char= ch #\=) (setf padding t))
                   (padding (return-from %base64-decode nil))
                   (t (let ((v (position ch +base64-alphabet+)))
                        (unless v (return-from %base64-decode nil))
                        (setf acc (logior (ash (logand acc #xFFFF) 6) v))
                        (incf bits 6)
                        (when (>= bits 8)
                          (decf bits 8)
                          (push (ldb (byte 8 bits) acc) out))))))
    (%octets (nreverse out))))
