(in-package #:terminal-backend-lisp)

;;; The backend object. Loaded last so every method exists before the
;;; load-time default below runs.

(defclass lisp-backend (tp:terminal-backend) ()
  (:documentation "Pure Common Lisp terminal-protocol backend."))

(defparameter +supported-features+ '(:scrollback :alternate-screen :osc-52 :format-vt))

(defmethod tp:backend-supports-p ((backend lisp-backend) feature)
  (and (member feature +supported-features+) t))

(defun %checked (name value type)
  (loop until (typep value type)
        do (setf value (%invalid-argument name value nil)))
  value)

(defun %rgb-arg (name value default)
  (if (null value) default (%checked name value 'tp:rgb)))

(defun %palette-arg (palette)
  (if (null palette)
      (%make-default-palette)
      (loop
        (when (and (typep palette 'sequence)
                   (= (length palette) 256)
                   (every #'tp:rgb-p palette))
          (return (coerce palette 'simple-vector)))
        (setf palette (%invalid-argument 'palette palette nil)))))

(defmethod tp:backend-make-terminal ((backend lisp-backend)
                                     &key cols rows host (scrollback-lines 10000)
                                       palette foreground background cursor-color
                                     &allow-other-keys)
  (make-instance 'lisp-terminal
                 :backend backend
                 :host host
                 :cols cols
                 :rows rows
                 :scrollback-lines (%checked 'scrollback-lines scrollback-lines '(integer 0))
                 :palette (%palette-arg palette)
                 :foreground (%rgb-arg 'foreground foreground
                                       (%triple-rgb +default-foreground-triple+))
                 :background (%rgb-arg 'background background
                                       (%triple-rgb +default-background-triple+))
                 :cursor-color (%rgb-arg 'cursor-color cursor-color nil)))

;;; Become the default backend only if no other backend claimed it.
(unless tp:*terminal-backend*
  (setf tp:*terminal-backend* (make-instance 'lisp-backend)))
