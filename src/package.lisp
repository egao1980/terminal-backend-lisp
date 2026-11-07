(defpackage #:terminal-backend-lisp
  (:use #:cl)
  (:local-nicknames (#:tp #:terminal-protocol))
  (:export #:lisp-backend
           #:lisp-terminal))
