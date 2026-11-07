(defsystem "terminal-backend-lisp"
  :version "0.2.0"
  :description "Pure Common Lisp terminal-protocol backend (VT core subset)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "terminal-protocol" "0.2.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "palette")
               (:file "width")
               (:file "codec")
               (:file "grid")
               (:file "terminal")
               (:file "sgr")
               (:file "modes")
               (:file "control")
               (:file "kitty")
               (:file "parser")
               (:file "state")
               (:file "frame")
               (:file "input")
               (:file "backend"))
  :in-order-to ((test-op (test-op "terminal-backend-lisp/tests"))))

(defsystem "terminal-backend-lisp/tests"
  :depends-on ("terminal-backend-lisp" "terminal-parity/rove" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "unit-test")
               (:file "kitty-test")
               (:file "parity-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
