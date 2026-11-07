(in-package #:terminal-backend-lisp/tests)

;;; The contract: every applicable fixture in terminal-parity's corpus,
;;; fed whole and fed byte by byte, must match the libghostty-vt oracle.
(terminal-parity/rove:define-parity-tests lisp
  (make-instance 'tl:lisp-backend))
