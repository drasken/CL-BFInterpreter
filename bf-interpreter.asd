(asdf:defsystem #:bf-interpreter
  :description "Simple Brainfuck's interpreter using CommonLisp"
  :author "Drasken"
  :license "BSD"
  :version "0.1"
  :serial t
  :depends-on (#:uiop)
  :components ((:file "package")
	       (:file "main")
	       (:file "bf-interpreter")))
