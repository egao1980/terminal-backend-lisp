(in-package #:terminal-backend-lisp)

;;; The byte state machine (after Paul Williams' VT500 diagram). All
;;; in-flight state lives in the terminal's PARSER struct and %FEED-BYTE reads
;;; only its argument, so a feed split anywhere (inside ESC, CSI, OSC or a
;;; UTF-8 sequence) resumes exactly where it stopped.
;;;
;;; States: :ground :escape :escape-intermediate :csi-entry :csi-param
;;; :csi-intermediate :csi-ignore :osc-string :osc-esc :string-ignore
;;; :string-esc.
;;;
;;; APC (ESC _) reuses the OSC collector: PARSER-STRING-KIND tells the two
;;; apart. An APC ends with ST or BEL like an OSC, but ESC followed by any
;;; other byte aborts it instead of dispatching, so a half-received graphics
;;; command never runs. Over +MAX-OSC-BYTES+ the bytes are dropped and the
;;; overflow flag set; %APC-DISPATCH refuses such a command.
;;;
;;; Before an action that can reach the host runs, the next state is already
;;; committed, so a non-local exit from a host method leaves a consistent parser.

(defconstant +max-params+ 32)
(defconstant +max-param-value+ 65535)
(defconstant +max-osc-bytes+ (* 4 1024 1024))

(defun %reset-parser (p)
  (setf (parser-state p) :ground
        (fill-pointer (parser-params p)) 0
        (parser-current p) '()
        (parser-digits p) nil
        (parser-seen p) nil
        (parser-private p) nil
        (parser-intermediates p) '()
        (fill-pointer (parser-osc p)) 0
        (parser-osc-overflow p) nil
        (parser-string-kind p) :osc
        (parser-utf8-need p) 0))

(defun %csi-begin (p)
  (setf (parser-state p) :csi-entry
        (fill-pointer (parser-params p)) 0
        (parser-current p) '()
        (parser-digits p) nil
        (parser-seen p) nil
        (parser-private p) nil
        (parser-intermediates p) '()))

(defun %close-subparam (p)
  (push (parser-digits p) (parser-current p))
  (setf (parser-digits p) nil))

(defun %close-param (p)
  (%close-subparam p)
  (when (< (length (parser-params p)) +max-params+)
    (vector-push-extend (reverse (parser-current p)) (parser-params p)))
  (setf (parser-current p) '()))

(defun %finish-params (p)
  "→ a fresh simple-vector of the CSI's parameter lists."
  (when (parser-seen p) (%close-param p))
  (coerce (parser-params p) 'simple-vector))

(defun %csi-byte (term p b)
  "A byte in :CSI-ENTRY or :CSI-PARAM."
  (cond ((<= #x30 b #x39)
         (setf (parser-seen p) t
               (parser-digits p) (min +max-param-value+
                                      (+ (* 10 (or (parser-digits p) 0)) (- b #x30)))
               (parser-state p) :csi-param))
        ((= b #x3A) (setf (parser-seen p) t (parser-state p) :csi-param) (%close-subparam p))
        ((= b #x3B) (setf (parser-seen p) t (parser-state p) :csi-param) (%close-param p))
        ((<= #x3C b #x3F)
         (if (and (eq (parser-state p) :csi-entry) (not (parser-seen p)))
             (setf (parser-private p) (code-char b) (parser-state p) :csi-param)
             (setf (parser-state p) :csi-ignore)))
        ((<= #x20 b #x2F)
         (push (code-char b) (parser-intermediates p))
         (setf (parser-state p) :csi-intermediate))
        ((<= #x40 b #x7E) (%csi-final term p b))
        (t nil)))

(defun %csi-final (term p b)
  (let ((params (%finish-params p))
        (private (parser-private p))
        (intermediates (reverse (parser-intermediates p))))
    (setf (parser-state p) :ground)
    (%csi-dispatch term (code-char b) params private intermediates)))

(defun %utf8-byte (term p b)
  "A byte ≥ #x80 in :GROUND."
  (if (plusp (parser-utf8-need p))
      ;; Only continuation bytes reach here; see %FEED-BYTE.
      (progn
        (setf (parser-utf8-code p) (logior (ash (parser-utf8-code p) 6) (logand b #x3F)))
        (when (zerop (decf (parser-utf8-need p)))
          (let ((code (parser-utf8-code p)))
            (%print term (if (%valid-scalar-p code (parser-utf8-min p)) code #xFFFD)))))
      (multiple-value-bind (need code minimum) (%utf8-lead b)
        (if need
            (setf (parser-utf8-need p) need
                  (parser-utf8-code p) code
                  (parser-utf8-min p) minimum)
            (%print term #xFFFD)))))

(defun %string-dispatch (term terminator)
  "Run the OSC or APC just collected. The parser is already in :GROUND."
  (if (eq (parser-string-kind (%parser term)) :apc)
      (%apc-dispatch term)
      (%osc-dispatch term terminator)))

(defun %feed-byte (term b)
  (declare (type (unsigned-byte 8) b))
  (let ((p (%parser term)))
    ;; A partial UTF-8 sequence interrupted by anything but a continuation
    ;; byte becomes U+FFFD; the byte is then processed normally.
    (when (and (plusp (parser-utf8-need p)) (not (<= #x80 b #xBF)))
      (setf (parser-utf8-need p) 0)
      (%print term #xFFFD))
    (let ((state (parser-state p)))
      ;; Transitions from anywhere outside string states.
      (unless (member state '(:osc-string :osc-esc :string-ignore :string-esc))
        (case b
          ((#x18 #x1A)
           (setf (parser-state p) :ground (parser-utf8-need p) 0)
           (return-from %feed-byte))
          (#x1B
           (setf (parser-state p) :escape (parser-intermediates p) '())
           (return-from %feed-byte))))
      (ecase state
        (:ground
         (cond ((< b #x20) (%execute term b))
               ((< b #x7F) (%print term b))
               ((= b #x7F) nil)
               (t (%utf8-byte term p b))))
        (:escape
         (cond ((< b #x20) (%execute term b))
               ((<= #x20 b #x2F)
                (push (code-char b) (parser-intermediates p))
                (setf (parser-state p) :escape-intermediate))
               ((= b #x5B) (%csi-begin p))
               ((member b '(#x5D #x5F))   ; OSC, APC: collected
                (setf (fill-pointer (parser-osc p)) 0
                      (parser-osc-overflow p) nil
                      (parser-string-kind p) (if (= b #x5D) :osc :apc)
                      (parser-state p) :osc-string))
               ((member b '(#x50 #x58 #x5E)) ; DCS SOS PM: swallowed
                (setf (parser-state p) :string-ignore))
               ((<= #x30 b #x7E)
                (setf (parser-state p) :ground)
                (%esc-dispatch term (code-char b) nil))
               (t (setf (parser-state p) :ground))))
        (:escape-intermediate
         (cond ((< b #x20) (%execute term b))
               ((<= #x20 b #x2F) (push (code-char b) (parser-intermediates p)))
               ((<= #x30 b #x7E)
                (setf (parser-state p) :ground)
                (%esc-dispatch term (code-char b) (reverse (parser-intermediates p))))
               (t (setf (parser-state p) :ground))))
        ((:csi-entry :csi-param)
         (if (< b #x20) (%execute term b) (%csi-byte term p b)))
        (:csi-intermediate
         (cond ((< b #x20) (%execute term b))
               ((<= #x20 b #x2F) (push (code-char b) (parser-intermediates p)))
               ((<= #x30 b #x3F) (setf (parser-state p) :csi-ignore))
               ((<= #x40 b #x7E) (%csi-final term p b))
               (t nil)))
        (:csi-ignore
         (cond ((< b #x20) (%execute term b))
               ((<= #x40 b #x7E) (setf (parser-state p) :ground))
               (t nil)))
        (:osc-string
         (cond ((= b 7)
                (setf (parser-state p) :ground)
                (%string-dispatch term :bel))
               ((= b #x1B) (setf (parser-state p) :osc-esc))
               ((member b '(#x18 #x1A)) (setf (parser-state p) :ground))
               ((< b #x20) nil)
               ((>= (length (parser-osc p)) +max-osc-bytes+)
                (setf (parser-osc-overflow p) t))
               (t (vector-push-extend b (parser-osc p)))))
        (:osc-esc
         (setf (parser-state p) :ground)
         (when (or (= b #x5C) (eq (parser-string-kind p) :osc))
           (%string-dispatch term :st))
         (unless (= b #x5C)
           ;; ESC ended the OSC and starts a new sequence with this byte.
           (setf (parser-state p) :escape (parser-intermediates p) '())
           (%feed-byte term b)))
        (:string-ignore
         (cond ((= b #x1B) (setf (parser-state p) :string-esc))
               ((member b '(#x18 #x1A)) (setf (parser-state p) :ground))
               (t nil)))
        (:string-esc
         (if (= b #x5C)
             (setf (parser-state p) :ground)
             (progn (setf (parser-state p) :escape (parser-intermediates p) '())
                    (%feed-byte term b))))))))

(defmethod tp:terminal-feed ((term lisp-terminal) octets &key (start 0) end)
  (loop for i from start below (or end (length octets))
        do (%feed-byte term (aref octets i)))
  nil)
