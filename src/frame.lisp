(in-package #:terminal-backend-lisp)

;;; Frames: a snapshot of the viewport with resolved colours and dirty rows.
;;;
;;; A row is rebuilt when the line object shown at that y changed (scroll,
;;; screen switch, scrollback) or the line's version changed (an edit). A
;;; terminal epoch bump (resize, reset, viewport scroll, screen switch) makes
;;; the next update :FULL. Rows and the frame stay dirty until FRAME-MARK-CLEAN.

(defclass lisp-frame (tp:frame)
  ((cols :initform 0 :accessor %frame-cols)
   (rows :initform 0 :accessor %frame-rows)
   (row-objects :initform nil :accessor %frame-row-objects) ; simple-vector of tp:row
   (seen-lines :initform nil :accessor %frame-seen-lines)
   (seen-versions :initform nil :accessor %frame-seen-versions)
   (epoch :initform -1 :accessor %frame-epoch)
   (dirty :initform :none :accessor %frame-dirty) ; accumulated until mark-clean
   (cursor :initform nil :accessor %frame-cursor)
   (colors :initform nil :accessor %frame-colors)))

(defmethod tp:make-frame ((term lisp-terminal))
  (make-instance 'lisp-frame :terminal term))

(defun %resolve-color (color palette)
  (if (integerp color) (svref palette color) color))

(defun %resolve-style (style palette cache)
  (let ((fg (tp:style-foreground style))
        (bg (tp:style-background style))
        (ul (tp:style-underline-color style)))
    (if (not (or (integerp fg) (integerp bg) (integerp ul)))
        style
        (or (gethash style cache)
            (setf (gethash style cache)
                  (let ((copy (tp:copy-style style)))
                    (setf (tp:style-foreground copy) (%resolve-color fg palette)
                          (tp:style-background copy) (%resolve-color bg palette)
                          (tp:style-underline-color copy) (%resolve-color ul palette))
                    copy))))))

(defun %resolved-cells (line cols palette cache)
  "→ a fresh vector of COLS fresh cells for LINE (padded or cut to COLS)."
  (let ((cells (line-cells line))
        (out (make-array cols)))
    (dotimes (x cols)
      (let ((cell (if (< x (length cells)) (svref cells x) +blank-cell+)))
        (setf (svref out x)
              (tp:make-cell :text (tp:cell-text cell)
                            :width (if (and (= x (1- cols)) (eq (tp:cell-width cell) :wide))
                                       :narrow
                                       (tp:cell-width cell))
                            :style (%resolve-style (tp:cell-style cell) palette cache)))))
    out))

(defun %viewport-cursor (term)
  (let* ((s (%screen term))
         (vy (+ (screen-y s) (%viewport-offset term))))
    (when (< vy (%rows term))
      (%cursor-value term (screen-x s) vy))))

(defun %terminal-colors (term)
  (tp:make-terminal-colors :foreground (%foreground term)
                           :background (%background term)
                           :cursor (%cursor-color term)
                           :palette (copy-seq (%palette term))))

(defmethod tp:frame-update ((frame lisp-frame))
  (let* ((term (tp:frame-terminal frame))
         (cols (%cols term))
         (rows (%rows term))
         (palette (%palette term))
         (cache (make-hash-table :test #'eq))
         (full (or (null (%frame-row-objects frame))
                   (/= cols (%frame-cols frame))
                   (/= rows (%frame-rows frame))
                   (/= (%epoch term) (%frame-epoch frame))))
         (changed nil))
    (when full
      (setf (%frame-cols frame) cols
            (%frame-rows frame) rows
            (%frame-epoch frame) (%epoch term)
            (%frame-row-objects frame) (make-array rows :initial-element nil)
            (%frame-seen-lines frame) (make-array rows :initial-element nil)
            (%frame-seen-versions frame) (make-array rows :initial-element -1)))
    (let ((objects (%frame-row-objects frame))
          (seen (%frame-seen-lines frame))
          (versions (%frame-seen-versions frame)))
      (dotimes (y rows)
        (let ((line (%viewport-line term y)))
          (when (or full
                    (not (eq line (svref seen y)))
                    (/= (line-version line) (svref versions y)))
            (setf (svref objects y)
                  (make-instance 'tp:row :y y :dirty-p t
                                         :cells (%resolved-cells line cols palette cache))
                  (svref seen y) line
                  (svref versions y) (line-version line)
                  changed t)))))
    (setf (%frame-cursor frame) (%viewport-cursor term)
          (%frame-colors frame) (%terminal-colors term))
    ;; Like libghostty-vt's render state, dirtiness accumulates until
    ;; FRAME-MARK-CLEAN: :NONE < :PARTIAL < :FULL.
    (setf (%frame-dirty frame)
          (cond ((or full (eq (%frame-dirty frame) :full)) :full)
                ((or changed (eq (%frame-dirty frame) :partial)) :partial)
                (t :none)))))

(defmethod tp:frame-size ((frame lisp-frame))
  (values (%frame-cols frame) (%frame-rows frame)))

(defmethod tp:frame-cursor ((frame lisp-frame))
  ;; Built fresh by each FRAME-UPDATE.
  (%frame-cursor frame))

(defmethod tp:frame-colors ((frame lisp-frame))
  (let ((c (%frame-colors frame)))
    (and c (tp:make-terminal-colors :foreground (tp:terminal-colors-foreground c)
                                    :background (tp:terminal-colors-background c)
                                    :cursor (tp:terminal-colors-cursor c)
                                    :palette (copy-seq (tp:terminal-colors-palette c))))))

(defmethod tp:frame-row ((frame lisp-frame) y)
  (let ((rows (%frame-rows frame)))
    (loop until (and (integerp y) (< -1 y rows))
          do (setf y (%invalid-argument 'y y (tp:frame-terminal frame))))
    (svref (%frame-row-objects frame) y)))

(defmethod tp:frame-mark-clean ((frame lisp-frame))
  (let ((objects (%frame-row-objects frame)))
    (when objects
      (loop for row across objects
            when row do (setf (tp:row-dirty-p row) nil))))
  (setf (%frame-dirty frame) :none)
  nil)
