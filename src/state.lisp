(in-package #:terminal-backend-lisp)

;;; Protocol state queries, resize, reset, close, viewport scrolling and text.

(defun %invalid-argument (name value term)
  "Signal TP:TERMINAL-INVALID-ARGUMENT with a USE-VALUE restart. → the
replacement value."
  (restart-case (error 'tp:terminal-invalid-argument :name name :value value :terminal term)
    (use-value (replacement)
      :report "Supply a replacement value."
      :interactive (lambda ()
                     (format *query-io* "Replacement value: ")
                     (force-output *query-io*)
                     (list (read *query-io*)))
      replacement)))

(defmethod tp:terminal-size ((term lisp-terminal))
  (values (%cols term) (%rows term)))

(defun %cursor-value (term x y)
  (let ((s (%screen term)))
    (tp:make-cursor :x x :y y
                    :visible-p (and (%mode term :dec 25) t)
                    :style (%cursor-style term)
                    :blinking-p (and (%mode term :dec 12) t)
                    :wide-tail-p (eq (tp:cell-width (%cell-at s (screen-x s) (screen-y s)))
                                     :spacer))))

(defmethod tp:terminal-cursor ((term lisp-terminal))
  (let ((s (%screen term)))
    (%cursor-value term (screen-x s) (screen-y s))))

(defmethod tp:terminal-title ((term lisp-terminal)) (%title term))
(defmethod tp:terminal-pwd ((term lisp-terminal)) (%pwd term))
(defmethod tp:terminal-active-screen ((term lisp-terminal)) (%active-name term))

;;; ------------------------------------------------------------------
;;; Reset and close

(defmethod tp:terminal-reset ((term lisp-terminal))
  (%reset-parser (%parser term))
  (%full-reset term)
  nil)

(defmethod tp:terminal-close ((term lisp-terminal))
  ;; The protocol :AROUND marks the terminal closed; drop the storage.
  (setf (%primary term) nil
        (%alternate term) nil
        (%scrollback term) nil
        (%kitty term) nil
        (%parser term) nil)
  nil)

;;; ------------------------------------------------------------------
;;; Images (Kitty graphics; kitty.lisp)

(defmethod tp:terminal-image ((term lisp-terminal) image-id)
  (loop until (typep image-id '(integer 0))
        do (setf image-id (%invalid-argument 'image-id image-id term)))
  (let* ((store (%kitty term))
         (image (and store (gethash image-id (%ks-images store)))))
    (and image
         (tp:make-terminal-image :id (tp:terminal-image-id image)
                                 :width (tp:terminal-image-width image)
                                 :height (tp:terminal-image-height image)
                                 :format (tp:terminal-image-format image)
                                 :pixels (copy-seq (tp:terminal-image-pixels image))))))

;;; ------------------------------------------------------------------
;;; Resize (no reflow)

(defun %resize-line (line cols)
  "LINE with exactly COLS cells (a fresh line when the width changes)."
  (let ((cells (line-cells line)))
    (if (= (length cells) cols)
        line
        (let ((new (make-array cols :initial-element +blank-cell+)))
          (replace new cells)
          (when (and (plusp cols) (eq (tp:cell-width (svref new (1- cols))) :wide))
            (setf (svref new (1- cols)) +blank-cell+))
          (%make-line new)))))

(defun %resize-screen (term s cols rows feed-scrollback)
  (let* ((lines (map 'list (lambda (l) (%resize-line l cols)) (screen-lines s)))
         (y (screen-y s))
         (excess (- (length lines) rows)))
    ;; Shrinking: drop blank lines below the cursor first, then move lines off
    ;; the top (into scrollback on the primary screen), then cut the bottom.
    (when (plusp excess)
      (loop while (and (plusp excess) (> (length lines) (1+ y))
                       (%line-blank-p (car (last lines))))
            do (setf lines (butlast lines)) (decf excess))
      (let ((off-top (min excess y)))
        (when (plusp off-top)
          (let ((gone (subseq lines 0 off-top)))
            (when feed-scrollback (%push-scrollback term gone)))
          (setf lines (nthcdr off-top lines))
          (decf y off-top)
          (decf excess off-top)))
      (when (plusp excess)
        (setf lines (subseq lines 0 rows))))
    ;; Growing: append blank lines.
    (loop while (< (length lines) rows)
          do (setf lines (append lines (list (make-blank-line cols)))))
    (setf (screen-lines s) (coerce lines 'simple-vector)
          (screen-cols s) cols
          (screen-rows s) rows
          (screen-x s) (min (screen-x s) (1- cols))
          (screen-y s) (min y (1- rows))
          (screen-pending-wrap s) nil
          (screen-top s) 0
          (screen-bottom s) (1- rows)
          (screen-tabs s) (%default-tabs cols))
    (let ((saved (screen-saved s)))
      (when saved
        (setf (saved-cursor-x saved) (min (saved-cursor-x saved) (1- cols))
              (saved-cursor-y saved) (min (saved-cursor-y saved) (1- rows)))))))

(defun %pixel-arg (name value term)
  "NIL (keep the current size) or a non-negative integer, via USE-VALUE."
  (loop until (typep value '(or null (integer 0)))
        do (setf value (%invalid-argument name value term)))
  value)

(defmethod tp:terminal-resize ((term lisp-terminal) cols rows &key cell-width-px cell-height-px)
  ;; The cell pixel size is stored even when COLS × ROWS do not change: a
  ;; host may resize to the same grid only to report it.
  (let ((w (%pixel-arg 'cell-width-px cell-width-px term))
        (h (%pixel-arg 'cell-height-px cell-height-px term)))
    (when w (setf (%cell-width-px term) w))
    (when h (setf (%cell-height-px term) h)))
  (unless (and (= cols (%cols term)) (= rows (%rows term)))
    (%resize-screen term (%primary term) cols rows t)
    (%resize-screen term (%alternate term) cols rows nil)
    (setf (%cols term) cols (%rows term) rows)
    (setf (%offset term) (min (%offset term) (ring-count (%scrollback term))))
    (%bump-epoch term))
  nil)

;;; ------------------------------------------------------------------
;;; Viewport scrolling

(defmethod tp:terminal-scroll-viewport ((term lisp-terminal) &key to delta)
  (loop until (or (and to (null delta) (member to '(:top :bottom)))
                  (and delta (null to) (integerp delta)))
        do (let ((replacement (%invalid-argument (if to 'to 'delta) (or to delta) term)))
             (if (member replacement '(:top :bottom))
                 (setf to replacement delta nil)
                 (setf delta replacement to nil))))
  (let* ((n (%history-count term))
         (offset (cond ((eq to :top) n)
                       ((eq to :bottom) 0)
                       (t (%clamp (- (%offset term) delta) 0 n)))))
    (unless (= offset (%offset term))
      (setf (%offset term) offset)
      (%bump-epoch term)))
  nil)

;;; ------------------------------------------------------------------
;;; Text

(defun %region-lines (term region)
  (ecase region
    (:viewport (loop for y below (%rows term) collect (%viewport-line term y)))
    (:screen (coerce (screen-lines (%screen term)) 'list))
    (:scrollback (append (let ((ring (%scrollback term)))
                           (if (eq (%active-name term) :primary)
                               (loop for i below (ring-count ring) collect (ring-ref ring i))
                               '()))
                         (coerce (screen-lines (%screen term)) 'list)))))

(defun %plain-line (line)
  (let ((text (with-output-to-string (out)
                (loop for cell across (line-cells line)
                      unless (eq (tp:cell-width cell) :spacer)
                        do (let ((s (tp:cell-text cell)))
                             (write-string (if (string= s "") " " s) out))))))
    (string-right-trim " " text)))

(defun %sgr-color (out color base)
  "Write ';…' for COLOR as foreground (BASE 30), background (40) or underline (50)."
  (cond ((eq color :default))
        ((integerp color)
         (cond ((and (< color 8) (/= base 50)) (format out ";~D" (+ base color)))
               ((and (< color 16) (/= base 50)) (format out ";~D" (+ base 60 (- color 8))))
               (t (format out ";~D;5;~D" (+ base 8) color))))
        (t (format out ";~D;2;~D;~D;~D" (+ base 8)
                   (tp:rgb-r color) (tp:rgb-g color) (tp:rgb-b color)))))

(defun %sgr-string (style)
  (with-output-to-string (out)
    (format out "~C[0" #\Esc)
    (when (tp:style-bold style) (write-string ";1" out))
    (when (tp:style-faint style) (write-string ";2" out))
    (when (tp:style-italic style) (write-string ";3" out))
    (let ((u (tp:style-underline style)))
      (when u (format out ";4:~D" (position u +underline-kinds+))))
    (when (tp:style-blink style) (write-string ";5" out))
    (when (tp:style-inverse style) (write-string ";7" out))
    (when (tp:style-invisible style) (write-string ";8" out))
    (when (tp:style-strikethrough style) (write-string ";9" out))
    (when (tp:style-overline style) (write-string ";53" out))
    (%sgr-color out (tp:style-foreground style) 30)
    (%sgr-color out (tp:style-background style) 40)
    (%sgr-color out (tp:style-underline-color style) 50)
    (write-char #\m out)))

(defun %plain-style-p (style)
  (equalp style tp:+default-style+))

(defun %vt-line (line)
  (let* ((cells (line-cells line))
         (end (or (position-if (lambda (c)
                                 (or (not (%plain-style-p (tp:cell-style c)))
                                     (not (member (tp:cell-text c) '("" " ") :test #'string=))))
                               cells :from-end t)
                  -1)))
    (with-output-to-string (out)
      (let ((current tp:+default-style+))
        (loop for x from 0 to end
              for cell = (svref cells x)
              unless (eq (tp:cell-width cell) :spacer)
                do (let ((style (tp:cell-style cell)))
                     (unless (equalp style current)
                       (write-string (%sgr-string style) out)
                       (setf current style))
                     (let ((s (tp:cell-text cell)))
                       (write-string (if (string= s "") " " s) out))))
        (unless (%plain-style-p current)
          (format out "~C[0m" #\Esc))))))

(defun %join-lines (strings)
  (let ((strings (reverse strings)))
    (loop while (and strings (string= (first strings) ""))
          do (pop strings))
    (format nil "~{~A~^~%~}" (nreverse strings))))

(defmethod tp:terminal-text ((term lisp-terminal) &key (format :plain) (region :viewport))
  (let ((lines (%region-lines term region)))
    (case format
      (:plain (%join-lines (mapcar #'%plain-line lines)))
      (:vt (%join-lines (mapcar #'%vt-line lines)))
      (t (tp:unsupported :format-html term)))))
