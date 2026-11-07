(in-package #:terminal-backend-lisp)

;;; Actions the parser dispatches: print, C0 execute, ESC / CSI / OSC.
;;; Every host effect goes through TP:CALL-HOST.

;;; ------------------------------------------------------------------
;;; Print

(defun %wrap-line (term)
  "Soft wrap: mark the current line wrapped, CR, index."
  (let ((s (%screen term)))
    (setf (line-wrapped (%line-at s (screen-y s))) t
          (screen-x s) 0)
    (%index term)))

(defun %attach-combining (term code)
  "Join zero-width CODE to the grapheme before the cursor."
  (let* ((s (%screen term))
         (x (if (screen-pending-wrap s) (screen-x s) (1- (screen-x s))))
         (y (screen-y s)))
    (when (and (>= x 0) (eq (tp:cell-width (%cell-at s x y)) :spacer) (plusp x))
      (decf x))
    (when (>= x 0)
      (let ((old (%cell-at s x y)))
        (unless (string= (tp:cell-text old) "")
          (setf (svref (line-cells (%line-at s y)) x)
                (tp:make-cell :text (concatenate 'string (tp:cell-text old)
                                                 (string (code-char code)))
                              :width (tp:cell-width old)
                              :style (tp:cell-style old)))
          (%touch (%line-at s y)))))))

(defun %print (term code)
  (let* ((s (%screen term))
         (cols (screen-cols s))
         (width (%char-width code))
         (autowrap (%mode term :dec 7)))
    (when (zerop width)
      (return-from %print (%attach-combining term code)))
    (setf (%last-char term) code)
    (when (and (screen-pending-wrap s) autowrap)
      (%wrap-line term))
    (when (and (= width 2) (= (screen-x s) (1- cols)))
      ;; A wide character does not fit in the last column.
      (if (and autowrap (> cols 1))
          (progn (%put-cell s (screen-x s) (screen-y s) (%blank term))
                 (%wrap-line term))
          (return-from %print nil)))
    (when (%mode term :ansi 4)
      (%insert-cells s (screen-x s) (screen-y s) width (%blank term)))
    (let ((style (%cell-style term))
          (x (screen-x s)) (y (screen-y s)))
      (%put-cell s x y (tp:make-cell :text (string (code-char code))
                                     :width (if (= width 2) :wide :narrow)
                                     :style style))
      (when (= width 2)
        (%put-cell s (1+ x) y (tp:make-cell :width :spacer :style style))))
    (let ((nx (+ (screen-x s) width)))
      (if (>= nx cols)
          (setf (screen-x s) (1- cols)
                (screen-pending-wrap s) autowrap)
          (setf (screen-x s) nx
                (screen-pending-wrap s) nil)))))

;;; ------------------------------------------------------------------
;;; C0

(defun %next-tab (s)
  (let ((cols (screen-cols s)))
    (or (loop for x from (1+ (screen-x s)) below cols
              when (= 1 (sbit (screen-tabs s) x)) return x)
        (1- cols))))

(defun %previous-tab (s)
  (or (loop for x from (1- (screen-x s)) downto 1
            when (= 1 (sbit (screen-tabs s) x)) return x)
      0))

(defun %execute (term code)
  (let ((s (%screen term)))
    (case code
      (7 (tp:call-host term #'tp:host-bell))
      (8 (setf (screen-pending-wrap s) nil
               (screen-x s) (max 0 (1- (screen-x s)))))
      (9 (setf (screen-pending-wrap s) nil
               (screen-x s) (%next-tab s)))
      ((10 11 12) (%index term)
       (when (%mode term :ansi 20) (setf (screen-x s) 0)))
      (13 (setf (screen-pending-wrap s) nil
                (screen-x s) 0))
      (t nil))))

;;; ------------------------------------------------------------------
;;; ESC

(defun %esc-dispatch (term final intermediates)
  (when intermediates
    ;; Charset designations (ESC ( B …), DECALN and friends: ignored.
    (return-from %esc-dispatch nil))
  (let ((s (%screen term)))
    (case final
      (#\7 (%save-cursor term))
      (#\8 (%restore-cursor term))
      (#\D (%index term))
      (#\E (setf (screen-x s) 0) (%index term))
      (#\H (setf (sbit (screen-tabs s) (screen-x s)) 1))
      (#\M (%reverse-index term))
      (#\c (%full-reset term))
      (#\= (%set-mode term :dec 66 t))
      (#\> (%set-mode term :dec 66 nil))
      (t nil))))

;;; ------------------------------------------------------------------
;;; CSI

(defun %param (params i default)
  "Parameter I; empty or 0 means DEFAULT."
  (let ((v (and (< i (length params)) (first (aref params i)))))
    (if (or (null v) (zerop v)) default v)))

(defun %param0 (params i default)
  "Parameter I; only empty means DEFAULT (0 is a value)."
  (let ((v (and (< i (length params)) (first (aref params i)))))
    (or v default)))

(defun %clamp (v lo hi) (max lo (min hi v)))

(defun %move-to (term x y)
  "Absolute move in screen coordinates, clamped."
  (let ((s (%screen term)))
    (setf (screen-x s) (%clamp x 0 (1- (screen-cols s)))
          (screen-y s) (%clamp y 0 (1- (screen-rows s)))
          (screen-pending-wrap s) nil)))

(defun %cup (term row col)
  "CUP with 0-based ROW and COL, honouring origin mode."
  (let ((s (%screen term)))
    (if (%mode term :dec 6)
        (%move-to term col (%clamp (+ row (screen-top s)) (screen-top s) (screen-bottom s)))
        (%move-to term col row))))

(defun %cursor-up (term n)
  (let* ((s (%screen term))
         (limit (if (>= (screen-y s) (screen-top s)) (screen-top s) 0)))
    (%move-to term (screen-x s) (max limit (- (screen-y s) n)))))

(defun %cursor-down (term n)
  (let* ((s (%screen term))
         (limit (if (<= (screen-y s) (screen-bottom s)) (screen-bottom s) (1- (screen-rows s)))))
    (%move-to term (screen-x s) (min limit (+ (screen-y s) n)))))

(defun %erase-display (term mode)
  (let* ((s (%screen term)) (blank (%blank term))
         (x (screen-x s)) (y (screen-y s)) (cols (screen-cols s)))
    (setf (screen-pending-wrap s) nil)
    (case mode
      (0 (%erase-cells s y x cols blank)
         (loop for yy from (1+ y) below (screen-rows s) do (%erase-cells s yy 0 cols blank)))
      (1 (%erase-cells s y 0 (1+ x) blank)
         (loop for yy from 0 below y do (%erase-cells s yy 0 cols blank)))
      (2 (loop for yy from 0 below (screen-rows s) do (%erase-cells s yy 0 cols blank)))
      (3 (ring-clear (%scrollback term))
         (setf (%offset term) 0)
         (%bump-epoch term)))))

(defun %erase-line (term mode)
  (let* ((s (%screen term)) (blank (%blank term))
         (x (screen-x s)) (y (screen-y s)) (cols (screen-cols s)))
    (setf (screen-pending-wrap s) nil)
    (case mode
      (0 (%erase-cells s y x cols blank))
      (1 (%erase-cells s y 0 (1+ x) blank))
      (2 (%erase-cells s y 0 cols blank)))))

(defun %insert-lines (term n)
  (let ((s (%screen term)))
    (when (<= (screen-top s) (screen-y s) (screen-bottom s))
      (%scroll-down s (screen-y s) (screen-bottom s) n (%blank term))
      (setf (screen-x s) 0 (screen-pending-wrap s) nil))))

(defun %delete-lines (term n)
  (let ((s (%screen term)))
    (when (<= (screen-top s) (screen-y s) (screen-bottom s))
      (%scroll-up s (screen-y s) (screen-bottom s) n (%blank term))
      (setf (screen-x s) 0 (screen-pending-wrap s) nil))))

(defun %set-scroll-region (term params)
  (let* ((s (%screen term))
         (top (1- (%param params 0 1)))
         (bottom (1- (min (%param params 1 (screen-rows s)) (screen-rows s)))))
    (when (< top bottom)
      (setf (screen-top s) top (screen-bottom s) bottom)
      (%home term))))

(defun %device-status (term params)
  (case (%param0 params 0 0)
    (5 (%reply term (format nil "~C[0n" #\Esc)))
    (6 (let* ((s (%screen term))
              (row (if (%mode term :dec 6) (- (screen-y s) (screen-top s)) (screen-y s))))
         (%reply term (format nil "~C[~D;~DR" #\Esc (1+ row) (1+ (screen-x s))))))))

(defun %set-cursor-style (term n)
  (multiple-value-bind (style blink)
      (case n
        ((0 1) (values :block (= n 1)))
        (2 (values :block nil))
        (3 (values :underline t))
        (4 (values :underline nil))
        (5 (values :bar t))
        (6 (values :bar nil))
        (t (return-from %set-cursor-style nil)))
    (setf (%cursor-style term) style
          (gethash 12 (%dec-modes term)) blink)))

(defun %csi-private (term final params intermediates)
  (cond ((and (null intermediates) (member final '(#\h #\l)))
         (loop for p across params
               for n = (first p)
               when n do (%set-mode term :dec n (char= final #\h))))
        ((and (equal intermediates '(#\$)) (char= final #\p))
         (let ((n (%param0 params 0 0)))
           (%reply term (format nil "~C[?~D;~D$y" #\Esc n (%decrqm-value term :dec n)))))
        ((and (null intermediates) (char= final #\n) (= 6 (%param0 params 0 0)))
         (let* ((s (%screen term))
                (row (if (%mode term :dec 6) (- (screen-y s) (screen-top s)) (screen-y s))))
           (%reply term (format nil "~C[?~D;~DR" #\Esc (1+ row) (1+ (screen-x s))))))))

(defun %csi-dispatch (term final params private intermediates)
  (cond
    ((eql private #\?) (%csi-private term final params intermediates))
    (private nil)
    (intermediates
     (cond ((and (equal intermediates '(#\$)) (char= final #\p))
            (let ((n (%param0 params 0 0)))
              (%reply term (format nil "~C[~D;~D$y" #\Esc n (%decrqm-value term :ansi n)))))
           ((and (equal intermediates '(#\Space)) (char= final #\q))
            (%set-cursor-style term (%param0 params 0 0)))))
    (t
     (let* ((s (%screen term))
            (n (%param params 0 1)))
       (case final
         (#\@ (setf (screen-pending-wrap s) nil)
          (%insert-cells s (screen-x s) (screen-y s) n (%blank term)))
         (#\A (%cursor-up term n))
         (#\B (%cursor-down term n))
         (#\C (%move-to term (+ (screen-x s) n) (screen-y s)))
         (#\D (%move-to term (- (screen-x s) n) (screen-y s)))
         (#\E (%cursor-down term n) (setf (screen-x s) 0))
         (#\F (%cursor-up term n) (setf (screen-x s) 0))
         ((#\G #\`) (%move-to term (1- n) (screen-y s)))
         ((#\H #\f) (%cup term (1- (%param params 0 1)) (1- (%param params 1 1))))
         (#\I (dotimes (i n) (setf (screen-x s) (%next-tab s)))
          (setf (screen-pending-wrap s) nil))
         (#\J (%erase-display term (%param0 params 0 0)))
         (#\K (%erase-line term (%param0 params 0 0)))
         (#\L (%insert-lines term n))
         (#\M (%delete-lines term n))
         (#\P (setf (screen-pending-wrap s) nil)
          (%delete-cells s (screen-x s) (screen-y s) n (%blank term)))
         (#\S (%scroll-up s (screen-top s) (screen-bottom s) n (%blank term)))
         (#\T (%scroll-down s (screen-top s) (screen-bottom s) n (%blank term)))
         (#\X (setf (screen-pending-wrap s) nil)
          (%erase-cells s (screen-y s) (screen-x s) (+ (screen-x s) n) (%blank term)))
         (#\Z (dotimes (i n) (setf (screen-x s) (%previous-tab s)))
          (setf (screen-pending-wrap s) nil))
         (#\a (%move-to term (+ (screen-x s) n) (screen-y s)))
         (#\b (let ((code (%last-char term)))
                (when code (dotimes (i (min n 65535)) (%print term code)))))
         (#\c (when (zerop (%param0 params 0 0))
                (%reply term (format nil "~C[?62;22c" #\Esc))))
         (#\d (%cup term (1- n) (screen-x s)))
         (#\e (%cursor-down term n))
         (#\g (case (%param0 params 0 0)
                (0 (setf (sbit (screen-tabs s) (screen-x s)) 0))
                (3 (fill (screen-tabs s) 0))))
         ((#\h #\l) (loop for p across params
                          for m = (first p)
                          when m do (%set-mode term :ansi m (char= final #\h))))
         (#\m (%apply-sgr term params))
         (#\n (%device-status term params))
         (#\r (%set-scroll-region term params))
         (#\s (%save-cursor term))
         (#\u (%restore-cursor term))
         (t nil))))))

;;; ------------------------------------------------------------------
;;; OSC

(defun %split-at-semicolon (string)
  "→ (values before after), or NIL when STRING has no ';'."
  (let ((p (position #\; string)))
    (when p (values (subseq string 0 p) (subseq string (1+ p))))))

(defun %osc-52 (term rest terminator)
  (multiple-value-bind (selector data) (%split-at-semicolon rest)
    (when selector
      (let* ((letter (if (string= selector "") #\c (char selector 0)))
             (which (if (member letter '(#\p #\s)) :primary :clipboard)))
        (if (string= data "?")
            (let* ((answer (tp:call-host term #'tp:host-clipboard-read which))
                   (payload (if (stringp answer) (%base64-encode (%utf8-encode answer)) "")))
              (%reply term (format nil "~C]52;~C;~A~A" #\Esc letter payload
                                   (if (eq terminator :bel)
                                       (string (code-char 7))
                                       (format nil "~C\\" #\Esc)))))
            (let ((octets (%base64-decode data)))
              (when octets
                (tp:call-host term #'tp:host-clipboard-write which (%utf8-decode octets)))))))))

(defun %osc-dispatch (term terminator)
  "Dispatch the OSC collected in the parser. TERMINATOR is :BEL or :ST."
  (let ((p (%parser term)))
    (when (parser-osc-overflow p)
      (return-from %osc-dispatch nil))
    (let ((string (%utf8-decode (parser-osc p))))
      (multiple-value-bind (ps rest) (%split-at-semicolon string)
        (unless ps (return-from %osc-dispatch nil))
        (cond ((member ps '("0" "2") :test #'string=)
               (setf (%title term) rest)
               (tp:call-host term #'tp:host-title-changed rest))
              ((string= ps "7")
               (setf (%pwd term) rest)
               (tp:call-host term #'tp:host-pwd-changed rest))
              ((string= ps "8")
               (multiple-value-bind (params uri) (%split-at-semicolon rest)
                 (when params           ; "8;params;uri"; an empty URI ends the link
                   (setf (%hyperlink term) (if (string= uri "") nil uri)))))
              ((string= ps "52") (%osc-52 term rest terminator)))))))
