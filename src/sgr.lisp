(in-package #:terminal-backend-lisp)

;;; SGR (CSI … m). PARAMS is a vector of lists: each list is a parameter and
;;; its colon subparameters, NIL elements being empty. Colours stay
;;; unresolved (palette index, :DEFAULT or RGB); frames resolve them.

(defparameter +underline-kinds+ #(nil :single :double :curly :dotted :dashed))

(defun %byte-or-nil (value)
  (and (typep value '(integer 0 255)) value))

(defun %extended-color (params i)
  "Parse the colour that starts at parameter I (a 38 / 48 / 58).
→ (values colour-or-NIL next-index)."
  (let* ((param (aref params i))
         (subs (rest param)))
    (if subs
        ;; Colon form: 38:5:n, 38:2:r:g:b or 38:2:cs:r:g:b.
        (let ((kind (first subs)))
          (values
           (case kind
             (5 (%byte-or-nil (second subs)))
             (2 (let ((rgb (if (>= (length subs) 5) (cddr subs) (cdr subs))))
                  (destructuring-bind (&optional r g b &rest more) rgb
                    (declare (ignore more))
                    (let ((r (%byte-or-nil (or r 0)))
                          (g (%byte-or-nil (or g 0)))
                          (b (%byte-or-nil (or b 0))))
                      (and r g b (tp:make-rgb r g b))))))
             (t nil))
           (1+ i)))
        ;; Semicolon form: 38;5;n or 38;2;r;g;b.
        (flet ((arg (k) (and (< (+ i k) (length params))
                             (first (aref params (+ i k))))))
          (case (arg 1)
            (5 (values (%byte-or-nil (arg 2)) (+ i 3)))
            (2 (let ((r (%byte-or-nil (or (arg 2) 0)))
                     (g (%byte-or-nil (or (arg 3) 0)))
                     (b (%byte-or-nil (or (arg 4) 0))))
                 (values (and r g b (tp:make-rgb r g b)) (+ i 5))))
            (t (values nil (length params))))))))

(defun %apply-sgr (term params)
  "Install the pen PARAMS describe on the active screen."
  (let* ((s (%screen term))
         (style (tp:copy-style (screen-pen s)))
         (n (length params))
         (i 0))
    (when (zerop n)
      (setf (screen-pen s) tp:+default-style+)
      (return-from %apply-sgr))
    (loop while (< i n)
          do (let* ((param (aref params i))
                    (code (or (first param) 0)))
               (incf i)
               (case code
                 (0 (setf style (tp:copy-style tp:+default-style+)))
                 (1 (setf (tp:style-bold style) t))
                 (2 (setf (tp:style-faint style) t))
                 (3 (setf (tp:style-italic style) t))
                 (4 (let ((sub (second param)))
                      (setf (tp:style-underline style)
                            (cond ((null (rest param)) :single)
                                  ((and (integerp sub) (< sub (length +underline-kinds+)))
                                   (svref +underline-kinds+ sub))
                                  (t (tp:style-underline style))))))
                 ((5 6) (setf (tp:style-blink style) t))
                 (7 (setf (tp:style-inverse style) t))
                 (8 (setf (tp:style-invisible style) t))
                 (9 (setf (tp:style-strikethrough style) t))
                 (21 (setf (tp:style-underline style) :double))
                 (22 (setf (tp:style-bold style) nil (tp:style-faint style) nil))
                 (23 (setf (tp:style-italic style) nil))
                 (24 (setf (tp:style-underline style) nil))
                 (25 (setf (tp:style-blink style) nil))
                 (27 (setf (tp:style-inverse style) nil))
                 (28 (setf (tp:style-invisible style) nil))
                 (29 (setf (tp:style-strikethrough style) nil))
                 ((30 31 32 33 34 35 36 37) (setf (tp:style-foreground style) (- code 30)))
                 (39 (setf (tp:style-foreground style) :default))
                 ((40 41 42 43 44 45 46 47) (setf (tp:style-background style) (- code 40)))
                 (49 (setf (tp:style-background style) :default))
                 (53 (setf (tp:style-overline style) t))
                 (55 (setf (tp:style-overline style) nil))
                 (59 (setf (tp:style-underline-color style) :default))
                 ((90 91 92 93 94 95 96 97) (setf (tp:style-foreground style) (+ 8 (- code 90))))
                 ((100 101 102 103 104 105 106 107)
                  (setf (tp:style-background style) (+ 8 (- code 100))))
                 ((38 48 58)
                  (multiple-value-bind (color next) (%extended-color params (1- i))
                    (setf i next)
                    (when color
                      (case code
                        (38 (setf (tp:style-foreground style) color))
                        (48 (setf (tp:style-background style) color))
                        (58 (setf (tp:style-underline-color style) color)))))))))
    (setf (screen-pen s) (if (equalp style tp:+default-style+) tp:+default-style+ style))))
