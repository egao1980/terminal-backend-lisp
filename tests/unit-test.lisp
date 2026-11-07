(in-package #:terminal-backend-lisp/tests)

;;; Behaviour the parity corpus can't see: features, effects pushed to the
;;; host, OSC 52 / OSC 8, the default palette, frames and lifecycle.

(defun octets (string) (par:decode-input string))

(defmacro with-lisp ((var &rest keys) &body body)
  `(let ((tp:*terminal-backend* (make-instance 'tl:lisp-backend)))
     (tp:with-terminal (,var ,@keys) ,@body)))

(defclass recording-host (tp:buffering-host)
  ((events :initform '() :accessor events)
   (clipboard :initform nil :initarg :clipboard :reader clipboard)))

(defmethod tp:host-bell ((h recording-host) term)
  (declare (ignore term))
  (push :bell (events h)))

(defmethod tp:host-title-changed ((h recording-host) term title)
  (declare (ignore term))
  (push (list :title title) (events h)))

(defmethod tp:host-pwd-changed ((h recording-host) term pwd)
  (declare (ignore term))
  (push (list :pwd pwd) (events h)))

(defmethod tp:host-clipboard-write ((h recording-host) term which string)
  (declare (ignore term))
  (push (list :clipboard which string) (events h)))

(defmethod tp:host-clipboard-read ((h recording-host) term which)
  (declare (ignore term which))
  (clipboard h))

(defclass failing-host (tp:terminal-host) ())

(defmethod tp:host-bell ((h failing-host) term)
  (declare (ignore term))
  (error "bell broke"))

(deftest backend-features
  (let ((b (make-instance 'tl:lisp-backend)))
    (ok (tp:backend-supports-p b :scrollback))
    (ok (tp:backend-supports-p b :alternate-screen))
    (ok (tp:backend-supports-p b :osc-52))
    (ng (tp:backend-supports-p b :kitty-keyboard))
    (ng (tp:backend-supports-p b :kitty-zlib))
    (ng (tp:backend-supports-p b :osc-133))
    (ng (tp:backend-supports-p b :format-html))))

(deftest loading-does-not-steal-default
  ;; Loading the system sets *TERMINAL-BACKEND* only when it was NIL.
  (ok (typep tp:*terminal-backend* 'tp:terminal-backend)))

(deftest effects-reach-host
  (let ((host (make-instance 'recording-host)))
    (with-lisp (term :host host)
      (tp:terminal-feed term (octets "a{bel}{osc}2;T{bel}{osc}7;file://h/x{bel}"))
      (ok (equal (reverse (events host))
                 '(:bell (:title "T") (:pwd "file://h/x")))))))

(deftest osc-52-write
  ;; "aGk=" is base64 for "hi".
  (let ((host (make-instance 'recording-host)))
    (with-lisp (term :host host)
      (tp:terminal-feed term (octets "{osc}52;c;aGk={bel}"))
      (ok (equal (events host) '((:clipboard :clipboard "hi")))))))

(deftest osc-52-read
  (let ((host (make-instance 'recording-host :clipboard "hi")))
    (with-lisp (term :host host)
      (tp:terminal-feed term (octets "{osc}52;c;?{bel}"))
      ;; Same terminator as the request (libghostty-vt behaviour).
      (ok (equalp (tp:terminal-take-replies term) (octets "{osc}52;c;aGk={bel}"))))))

(deftest osc-52-read-denied
  (let ((host (make-instance 'recording-host :clipboard nil)))
    (with-lisp (term :host host)
      (tp:terminal-feed term (octets "{osc}52;c;?{bel}"))
      ;; A denied read still answers, with an empty payload.
      (ok (equalp (tp:terminal-take-replies term) (octets "{osc}52;c;{bel}"))))))

(deftest osc-8-hyperlink
  (with-lisp (term :cols 10 :rows 1)
    (tp:terminal-feed term (octets "{osc}8;;https://x.test/{st}ab{osc}8;;{st}c"))
    (let ((row (aref (getf (tp:terminal-snapshot term) :cells) 0)))
      (ok (equal (tp:style-hyperlink (tp:cell-style (aref row 0))) "https://x.test/"))
      (ok (equal (tp:style-hyperlink (tp:cell-style (aref row 1))) "https://x.test/"))
      (ng (tp:style-hyperlink (tp:cell-style (aref row 2)))))))

(deftest host-errors-are-deferred
  (with-lisp (term :host (make-instance 'failing-host))
    (ok (signals (tp:terminal-feed term (octets "a{bel}b")) 'tp:terminal-host-error))
    (ok (equal (tp:terminal-text term) "ab"))))

(deftest default-palette-matches-oracle
  (let ((expected (with-open-file (in (par:corpus-pathname "palette.sexp"))
                    (with-standard-io-syntax (read in)))))
    (with-lisp (term)
      (tp:with-frame (frame term)
        (let* ((colors (tp:frame-colors frame))
               (palette (map 'list (lambda (c) (list (tp:rgb-r c) (tp:rgb-g c) (tp:rgb-b c)))
                             (tp:terminal-colors-palette colors))))
          (ok (equal palette (getf expected :palette)))
          (ok (equalp (tp:terminal-colors-foreground colors)
                      (apply #'tp:make-rgb (getf expected :foreground))))
          (ok (equalp (tp:terminal-colors-background colors)
                      (apply #'tp:make-rgb (getf expected :background)))))))))

(deftest frames-dirty-rows
  (with-lisp (term :cols 10 :rows 3)
    (tp:with-frame (frame term)
      (ok (eq :full (tp:frame-update frame)))
      (tp:frame-mark-clean frame)
      (ok (eq :none (tp:frame-update frame)))
      (tp:terminal-feed term (octets "{csi}2;1Hx"))
      (ok (eq :partial (tp:frame-update frame)))
      (let ((dirty '()))
        (tp:do-frame-rows (row frame :dirty-only t) (push (tp:row-y row) dirty))
        ;; Row 1 changed; the row the cursor left may be dirty too.
        (ok (member 1 dirty))
        (ng (member 2 dirty)))
      (ok (equalp (tp:frame-cursor frame) (tp:make-cursor :x 1 :y 1))))))

(deftest scrollback-and-viewport
  (with-lisp (term :cols 5 :rows 2)
    (tp:terminal-feed term (octets "1{cr}{lf}2{cr}{lf}3{cr}{lf}4"))
    (ok (equal (tp:terminal-text term) (format nil "3~%4")))
    (tp:terminal-scroll-viewport term :delta -2)
    (ok (equal (tp:terminal-text term) (format nil "1~%2")))
    (tp:terminal-scroll-viewport term :to :bottom)
    (ok (equal (tp:terminal-text term) (format nil "3~%4")))
    (ok (search "1" (tp:terminal-text term :region :scrollback)))))

(deftest text-formats
  (with-lisp (term :cols 10 :rows 2)
    (tp:terminal-feed term (octets "{csi}1mB{csi}0m"))
    (ok (search "B" (tp:terminal-text term :format :vt :region :screen)))
    (ok (signals (tp:terminal-text term :format :html) 'tp:terminal-unsupported))))

(deftest reset-and-close
  (let* ((tp:*terminal-backend* (make-instance 'tl:lisp-backend))
         (term (tp:make-terminal :cols 10 :rows 2)))
    (tp:terminal-feed term (octets "{osc}2;t{bel}abc"))
    (tp:terminal-reset term)
    (ok (equal (tp:terminal-text term) ""))
    (ng (tp:terminal-title term))
    (tp:terminal-close term)
    (ok (signals (tp:terminal-feed term (octets "x")) 'tp:terminal-closed-error))))
