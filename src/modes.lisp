(in-package #:terminal-backend-lisp)

;;; DEC private and ANSI modes: storage, DECSET / DECRST side effects, DECRQM
;;; and the protocol's BACKEND-MODE / TERMINAL-MOUSE-TRACKING. The recognised
;;; set is TP:+MODE-NAMES+; other numbers read as NIL and ignore writes.

(defparameter +mouse-event-modes+ '(9 1000 1002 1003))

(defun %save-cursor (term)
  (let ((s (%screen term)))
    (setf (screen-saved s)
          (make-saved-cursor :x (screen-x s) :y (screen-y s)
                             :pending-wrap (screen-pending-wrap s)
                             :pen (screen-pen s)
                             :origin (%mode term :dec 6)))))

(defun %restore-cursor (term)
  (let* ((s (%screen term))
         (saved (or (screen-saved s) (make-saved-cursor))))
    (setf (screen-x s) (min (saved-cursor-x saved) (1- (screen-cols s)))
          (screen-y s) (min (saved-cursor-y saved) (1- (screen-rows s)))
          (screen-pending-wrap s) (and (saved-cursor-pending-wrap saved)
                                       (= (saved-cursor-x saved) (1- (screen-cols s))))
          (screen-pen s) (saved-cursor-pen saved)
          (gethash 6 (%dec-modes term)) (saved-cursor-origin saved))))

(defun %clear-screen (screen)
  (dotimes (y (screen-rows screen))
    (setf (svref (screen-lines screen) y) (make-blank-line (screen-cols screen)))))

(defun %switch-screen (term which &key clear)
  "Make WHICH (:PRIMARY / :ALTERNATE) active, carrying the cursor position
and pen over. With CLEAR, blank the alternate screen first."
  (unless (eq which (%active-name term))
    (let ((from (%screen term))
          (to (if (eq which :primary) (%primary term) (%alternate term))))
      (when (and clear (eq which :alternate))
        (%clear-screen to))
      (setf (screen-x to) (screen-x from)
            (screen-y to) (screen-y from)
            (screen-pending-wrap to) (screen-pending-wrap from)
            (screen-pen to) (screen-pen from)
            (%active-name term) which)
      (%bump-epoch term))))

(defun %home (term)
  (let ((s (%screen term)))
    (setf (screen-x s) 0
          (screen-y s) (if (%mode term :dec 6) (screen-top s) 0)
          (screen-pending-wrap s) nil)))

(defun %set-dec-mode (term number value)
  (let ((value (and value t)))
    (case number
      ((9 1000 1002 1003)
       (when value
         (dolist (m +mouse-event-modes+) (remhash m (%dec-modes term)))))
      ((1005 1006 1015 1016)
       (when value
         (dolist (m '(1005 1006 1015 1016)) (remhash m (%dec-modes term))))))
    (case number
      (47 (%switch-screen term (if value :alternate :primary)))
      (1047 (if value
                (%switch-screen term :alternate)
                (progn (when (eq (%active-name term) :alternate)
                         (%clear-screen (%alternate term)))
                       (%switch-screen term :primary))))
      (1048 (if value (%save-cursor term) (%restore-cursor term)))
      (1049 (if value
                (unless (eq (%active-name term) :alternate)
                  (%save-cursor term)
                  (%switch-screen term :alternate :clear t))
                (when (eq (%active-name term) :alternate)
                  (%clear-screen (%alternate term))
                  (%switch-screen term :primary)
                  (%restore-cursor term)))))
    (setf (gethash number (%dec-modes term)) value)
    (case number
      (6 (%home term))
      (7 (unless value (setf (screen-pending-wrap (%screen term)) nil)))
      (25 (%bump-epoch term)))
    value))

(defun %set-mode (term kind number value)
  "Set mode NUMBER of KIND, running its side effects. Unknown modes are
ignored. → VALUE."
  (when (%known-mode-p kind number)
    (if (eq kind :dec)
        (%set-dec-mode term number value)
        (setf (gethash number (%ansi-modes term)) (and value t))))
  value)

(defun %decrqm-value (term kind number)
  "DECRQM answer: 1 set, 2 reset, 0 not recognised."
  (cond ((not (%known-mode-p kind number)) 0)
        ((%mode term kind number) 1)
        (t 2)))

(defmethod tp:backend-mode ((term lisp-terminal) kind number)
  (and (%known-mode-p kind number) (%mode term kind number)))

(defmethod (setf tp:backend-mode) (value (term lisp-terminal) kind number)
  (%set-mode term kind number value))

(defmethod tp:terminal-mouse-tracking ((term lisp-terminal))
  (cond ((%mode term :dec 1003) :any)
        ((%mode term :dec 1002) :button)
        ((%mode term :dec 1000) :normal)
        ((%mode term :dec 9) :x10)
        (t :none)))
