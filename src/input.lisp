(in-package #:terminal-backend-lisp)

;;; Terminal → program input encoding: legacy keys with xterm modifiers,
;;; mouse reports (X10 / UTF-8 / SGR / urxvt / SGR-pixels), focus and paste.
;;; Every result is a fresh octet vector, empty when nothing is sent.

(defun %bytes (&rest parts)
  "PARTS are strings, characters or octets → OCTETS (strings UTF-8 encoded)."
  (let ((out '()))
    (dolist (part parts)
      (etypecase part
        (string (loop for ch across part do (setf out (%utf8-push (char-code ch) out))))
        (character (setf out (%utf8-push (char-code part) out)))
        (integer (push part out))))
    (%octets (nreverse out))))

(defun %csi (&rest parts)
  (apply #'%bytes #\Esc #\[ parts))

(defun %modifier-param (modifiers)
  "xterm modifier parameter: 1 + shift + 2·alt + 4·ctrl + 8·super."
  (+ 1
     (if (member :shift modifiers) 1 0)
     (if (member :alt modifiers) 2 0)
     (if (member :ctrl modifiers) 4 0)
     (if (member :super modifiers) 8 0)))

;;; ------------------------------------------------------------------
;;; Keys

(defparameter +cursor-keys+
  '((:arrow-up . #\A) (:arrow-down . #\B) (:arrow-right . #\C) (:arrow-left . #\D)
    (:home . #\H) (:end . #\F)
    (:numpad-up . #\A) (:numpad-down . #\B) (:numpad-right . #\C) (:numpad-left . #\D)
    (:numpad-home . #\H) (:numpad-end . #\F))
  "Keys sent as CSI x / SS3 x (DECCKM) / CSI 1;m x.")

(defparameter +ss3-function-keys+
  '((:f1 . #\P) (:f2 . #\Q) (:f3 . #\R) (:f4 . #\S)))

(defparameter +tilde-keys+
  '((:insert . 2) (:delete . 3) (:page-up . 5) (:page-down . 6)
    (:numpad-insert . 2) (:numpad-delete . 3) (:numpad-page-up . 5) (:numpad-page-down . 6)
    (:f5 . 15) (:f6 . 17) (:f7 . 18) (:f8 . 19) (:f9 . 20) (:f10 . 21)
    (:f11 . 23) (:f12 . 24))
  "Keys sent as CSI n ~ / CSI n;m ~.")

(defun %letter-key-char (key)
  "The lowercase letter for key :A … :Z, or NIL."
  (let ((name (symbol-name key)))
    (and (= (length name) 1) (alpha-char-p (char name 0))
         (char-downcase (char name 0)))))

(defun %ctrl-code (char)
  "C0 byte for Ctrl+CHAR, or NIL."
  (let ((code (char-code char)))
    (cond ((char= char #\Space) 0)
          ((char= char #\?) #x7F)
          ((<= (char-code #\a) code (char-code #\z)) (- code 96))
          ((<= #x40 code #x5F) (- code #x40))
          ((char= char #\2) 0)
          ((<= (char-code #\3) code (char-code #\7)) (- code (char-code #\3) -27))
          ((char= char #\8) #x7F)
          (t nil))))

(defun %encode-text (text modifiers)
  (let ((alt (member :alt modifiers))
        (ctrl (member :ctrl modifiers)))
    (let ((body (if (and ctrl (= (length text) 1) (%ctrl-code (char text 0)))
                    (%bytes (%ctrl-code (char text 0)))
                    (%bytes text))))
      (if alt (concatenate 'octets (%bytes #\Esc) body) body))))

(defun %with-alt (modifiers octets)
  (if (member :alt modifiers) (concatenate 'octets (%bytes #\Esc) octets) octets))

(defmethod tp:encode-key ((term lisp-terminal) key &key text modifiers (action :press))
  (when (eq action :release)
    (return-from tp:encode-key (%empty-octets)))
  (let ((m (%modifier-param modifiers))
        (cursor-entry (assoc key +cursor-keys+))
        (ss3-entry (assoc key +ss3-function-keys+))
        (tilde-entry (assoc key +tilde-keys+)))
    (cond
      (cursor-entry
       (cond ((> m 1) (%csi (format nil "1;~D~C" m (cdr cursor-entry))))
             ((%mode term :dec 1) (%bytes #\Esc #\O (cdr cursor-entry)))
             (t (%csi (cdr cursor-entry)))))
      (ss3-entry
       (if (> m 1)
           (%csi (format nil "1;~D~C" m (cdr ss3-entry)))
           (%bytes #\Esc #\O (cdr ss3-entry))))
      (tilde-entry
       (if (> m 1)
           (%csi (format nil "~D;~D~~" (cdr tilde-entry) m))
           (%csi (format nil "~D~~" (cdr tilde-entry)))))
      ((member key '(:enter :numpad-enter))
       (%with-alt modifiers (if (%mode term :ansi 20) (%bytes 13 10) (%bytes 13))))
      ((eq key :tab)
       (if (member :shift modifiers) (%csi #\Z) (%with-alt modifiers (%bytes 9))))
      ((member key '(:backspace :numpad-backspace))
       (%with-alt modifiers (%bytes (if (member :ctrl modifiers) 8 #x7F))))
      ((eq key :escape) (%with-alt modifiers (%bytes 27)))
      ((and text (plusp (length text))) (%encode-text text modifiers))
      ((and (eq key :space) (member :ctrl modifiers)) (%with-alt modifiers (%bytes 0)))
      ((and (%letter-key-char key) (or (member :ctrl modifiers) (member :alt modifiers)))
       (%encode-text (string (%letter-key-char key)) modifiers))
      (t (%empty-octets)))))

;;; ------------------------------------------------------------------
;;; Mouse

(defparameter +mouse-button-codes+
  '((:left . 0) (:middle . 1) (:right . 2) (nil . 3)
    (:wheel-up . 64) (:wheel-down . 65) (:wheel-left . 66) (:wheel-right . 67)
    (:back . 128) (:forward . 129) (:button-10 . 130) (:button-11 . 131)))

(defun %wheel-p (button)
  (member button '(:wheel-up :wheel-down :wheel-left :wheel-right)))

(defun %mouse-reported-p (tracking button action)
  (ecase tracking
    (:none nil)
    (:x10 (eq action :press))
    (:normal (member action '(:press :release)))
    (:button (or (member action '(:press :release)) (and (eq action :motion) button)))
    (:any t)))

(defmethod tp:encode-mouse ((term lisp-terminal) button action x y
                            &key modifiers pixel-x pixel-y)
  (let ((tracking (tp:terminal-mouse-tracking term)))
    (unless (and (%mouse-reported-p tracking button action)
                 (not (and (eq action :release) (%wheel-p button))))
      (return-from tp:encode-mouse (%empty-octets)))
    (let* ((code (cdr (assoc button +mouse-button-codes+)))
           (code (+ code
                    (if (eq action :motion) 32 0)
                    (if (eq tracking :x10)
                        0
                        (+ (if (member :shift modifiers) 4 0)
                           (if (member :alt modifiers) 8 0)
                           (if (member :ctrl modifiers) 16 0)))))
           (final (if (eq action :release) #\m #\M)))
      (cond
        ((%mode term :dec 1006)
         (%csi (format nil "<~D;~D;~D~C" code (1+ x) (1+ y) final)))
        ((%mode term :dec 1016)
         (%csi (format nil "<~D;~D;~D~C" code (or pixel-x (1+ x)) (or pixel-y (1+ y)) final)))
        (t
         ;; Legacy formats encode a release as button 3.
         (let ((code (if (eq action :release) (+ 3 (logand code (lognot 3))) code)))
           (cond
             ((%mode term :dec 1015)
              (%csi (format nil "~D;~D;~DM" (+ 32 code) (1+ x) (1+ y))))
             ((%mode term :dec 1005)
              (if (> (+ 33 (max x y)) #x7FF)
                  (%empty-octets)
                  (%csi #\M (code-char (+ 32 code)) (code-char (+ 33 x)) (code-char (+ 33 y)))))
             ((> (+ 33 (max x y)) 255) (%empty-octets))
             (t (%csi #\M (+ 32 code) (+ 33 x) (+ 33 y))))))))))

;;; ------------------------------------------------------------------
;;; Focus and paste

(defmethod tp:encode-focus ((term lisp-terminal) focused-p)
  (if (%mode term :dec 1004)
      (%csi (if focused-p #\I #\O))
      (%empty-octets)))

(defmethod tp:encode-paste ((term lisp-terminal) string)
  (if (%mode term :dec 2004)
      (%csi "200~" string #\Esc #\[ "201~")
      (%bytes (substitute #\Return #\Newline string))))
