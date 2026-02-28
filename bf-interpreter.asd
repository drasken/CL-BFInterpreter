(asdf:defsystem #:bf-interpreter
  :description "Simple Brainfuck's interpreter using CommonLisp"
  :author "Drasken"
  :license "BSD"
  :version "0.1"
  :serial t
  ;; Binary building instructions START
  :build-operation "program-op"
  :build-pathname "bf-int"
  :entry-point "bf-interpreter:main"
  ; Binary building instructions END
  :depends-on (#:uiop)
  :components ((:file "package")
	       (:file "bf-interpreter")
	       (:file "main")))
