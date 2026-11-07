(in-package #:terminal-backend-lisp)

;;; Screen grid: lines of immutable TP:CELL values, a scrollback ring and the
;;; edit primitives. Nothing here knows about the terminal, the parser or the
;;; host.
;;;
;;; Invariants:
;;; - Cells and styles stored in a line are never mutated; edits replace them.
;;; - Every change to a line's cells bumps its VERSION (frames diff on it).
;;; - A :WIDE cell at x has a :SPACER at x+1, and every :SPACER follows a :WIDE.

(defparameter +blank-cell+ (tp:make-cell)
  "The cell nothing has painted. Shared; never mutated.")

(defstruct (line (:constructor %make-line (cells)))
  (cells #() :type simple-vector)
  (wrapped nil :type boolean)
  (version 0 :type fixnum))

(defun make-blank-line (cols &optional (blank +blank-cell+))
  (%make-line (make-array cols :initial-element blank)))

(declaim (inline %touch))
(defun %touch (line)
  (setf (line-version line)
        (logand (1+ (line-version line)) most-positive-fixnum))
  line)

(defun %line-blank-p (line)
  (every (lambda (c) (eq c +blank-cell+)) (line-cells line)))

;;; ------------------------------------------------------------------
;;; Saved cursor (DECSC) and screens

(defstruct saved-cursor
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (pending-wrap nil :type boolean)
  (pen tp:+default-style+ :type tp:style)
  (origin nil :type boolean))

(defstruct (screen (:constructor %make-screen))
  (lines #() :type simple-vector)
  (cols 0 :type fixnum)
  (rows 0 :type fixnum)
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (pending-wrap nil :type boolean)       ; ⇒ x = cols-1
  (pen tp:+default-style+ :type tp:style) ; immutable; SGR replaces it
  (top 0 :type fixnum)                    ; scroll region, inclusive
  (bottom 0 :type fixnum)
  (tabs #* :type simple-bit-vector)
  (saved nil :type (or null saved-cursor)))

(defun %default-tabs (cols)
  (let ((tabs (make-array cols :element-type 'bit :initial-element 0)))
    (loop for x from 8 below cols by 8 do (setf (sbit tabs x) 1))
    tabs))

(defun make-screen (cols rows)
  (let ((lines (make-array rows)))
    (dotimes (y rows) (setf (svref lines y) (make-blank-line cols)))
    (%make-screen :lines lines :cols cols :rows rows
                  :bottom (1- rows) :tabs (%default-tabs cols))))

(declaim (inline %line-at))
(defun %line-at (screen y)
  (svref (screen-lines screen) y))

(defun %cell-at (screen x y)
  (svref (line-cells (%line-at screen y)) x))

;;; ------------------------------------------------------------------
;;; Cell primitives

(defun %put-cell (screen x y cell)
  "Store CELL at X, Y, blanking the orphaned half of a wide pair."
  (let* ((line (%line-at screen y))
         (cells (line-cells line))
         (cols (length cells))
         (old (svref cells x)))
    (case (tp:cell-width old)
      (:wide (when (and (< (1+ x) cols)
                        (eq (tp:cell-width (svref cells (1+ x))) :spacer))
               (setf (svref cells (1+ x)) +blank-cell+)))
      (:spacer (when (and (plusp x)
                          (eq (tp:cell-width (svref cells (1- x))) :wide))
                 (setf (svref cells (1- x)) +blank-cell+))))
    (setf (svref cells x) cell)
    (%touch line)))

(defun %repair-edges (cells start end)
  "After CELLS[START, END) changed, blank a wide head left at END-1's left
edge or a spacer orphaned at END."
  (let ((cols (length cells)))
    (when (and (plusp start) (< start cols)
               (eq (tp:cell-width (svref cells (1- start))) :wide)
               (not (eq (tp:cell-width (svref cells start)) :spacer)))
      (setf (svref cells (1- start)) +blank-cell+))
    (when (and (< end cols)
               (eq (tp:cell-width (svref cells end)) :spacer)
               (or (zerop end)
                   (not (eq (tp:cell-width (svref cells (1- end))) :wide))))
      (setf (svref cells end) +blank-cell+))
    (loop for x from start below (min end cols)
          for width = (tp:cell-width (svref cells x))
          when (or (and (eq width :wide)
                        (or (= (1+ x) cols)
                            (not (eq (tp:cell-width (svref cells (1+ x))) :spacer))))
                   (and (eq width :spacer)
                        (or (zerop x)
                            (not (eq (tp:cell-width (svref cells (1- x))) :wide)))))
            do (setf (svref cells x) +blank-cell+))))

(defun %erase-cells (screen y start end blank)
  "Fill line Y's cells [START, END) with BLANK."
  (let* ((line (%line-at screen y))
         (cells (line-cells line))
         (end (min end (length cells))))
    (when (< start end)
      (fill cells blank :start start :end end)
      (%repair-edges cells start end)
      (%touch line))))

(defun %insert-cells (screen x y n blank)
  "Shift line Y right from X by N cells, filling the gap with BLANK."
  (let* ((line (%line-at screen y))
         (cells (line-cells line))
         (cols (length cells))
         (n (min n (- cols x))))
    (when (plusp n)
      (replace cells cells :start1 (+ x n) :start2 x :end2 (- cols n))
      (fill cells blank :start x :end (+ x n))
      (%repair-edges cells x cols)
      (when (and (plusp cols)
                 (eq (tp:cell-width (svref cells (1- cols))) :wide))
        (setf (svref cells (1- cols)) blank))
      (%touch line))))

(defun %delete-cells (screen x y n blank)
  "Delete N cells at X on line Y, shifting the rest left and filling with BLANK."
  (let* ((line (%line-at screen y))
         (cells (line-cells line))
         (cols (length cells))
         (n (min n (- cols x))))
    (when (plusp n)
      (replace cells cells :start1 x :start2 (+ x n))
      (fill cells blank :start (- cols n))
      (%repair-edges cells x cols)
      (%touch line))))

;;; ------------------------------------------------------------------
;;; Line scrolling inside [TOP, BOTTOM]

(defun %scroll-up (screen top bottom n blank)
  "Scroll lines [TOP, BOTTOM] up by N, filling with fresh BLANK lines.
→ the list of lines that left the region, top first."
  (let* ((lines (screen-lines screen))
         (height (1+ (- bottom top)))
         (n (min n height))
         (gone (loop for y from top below (+ top n) collect (svref lines y))))
    (replace lines lines :start1 top :start2 (+ top n) :end2 (1+ bottom))
    (loop for y from (1+ (- bottom n)) to bottom
          do (setf (svref lines y) (make-blank-line (screen-cols screen) blank)))
    gone))

(defun %scroll-down (screen top bottom n blank)
  "Scroll lines [TOP, BOTTOM] down by N, filling the top with fresh BLANK lines."
  (let* ((lines (screen-lines screen))
         (height (1+ (- bottom top)))
         (n (min n height)))
    (replace lines lines :start1 (+ top n) :start2 top :end2 (1+ (- bottom n)))
    (loop for y from top below (+ top n)
          do (setf (svref lines y) (make-blank-line (screen-cols screen) blank)))
    nil))

;;; ------------------------------------------------------------------
;;; Scrollback ring

(defstruct (ring (:constructor make-ring (capacity)))
  (capacity 0 :type (integer 0))
  (items (make-array 0) :type simple-vector)
  (start 0 :type fixnum)
  (count 0 :type fixnum))

(defun ring-push (ring item)
  "Append ITEM. → T if the oldest item was evicted."
  (let ((cap (ring-capacity ring)))
    (cond ((zerop cap) t)
          ((< (ring-count ring) (length (ring-items ring)))
           (setf (svref (ring-items ring)
                        (mod (+ (ring-start ring) (ring-count ring)) (length (ring-items ring))))
                 item)
           (incf (ring-count ring))
           nil)
          ((< (length (ring-items ring)) cap)
           ;; Grow by doubling (items are contiguous from START).
           (let* ((old (ring-items ring))
                  (new (make-array (min cap (max 16 (* 2 (length old)))) :initial-element nil)))
             (dotimes (i (ring-count ring))
               (setf (svref new i) (svref old (mod (+ (ring-start ring) i) (length old)))))
             (setf (ring-items ring) new (ring-start ring) 0)
             (ring-push ring item)))
          (t
           (setf (svref (ring-items ring) (ring-start ring)) item
                 (ring-start ring) (mod (1+ (ring-start ring)) cap))
           t))))

(defun ring-ref (ring i)
  "Item I, 0 = oldest."
  (svref (ring-items ring) (mod (+ (ring-start ring) i) (length (ring-items ring)))))

(defun ring-clear (ring)
  (setf (ring-items ring) (make-array 0) (ring-start ring) 0 (ring-count ring) 0))
