(asdf:defsystem #:let-alist
  :description "Common Lisp macro let-alist modeled by Emacs lisp macro of same name."
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "GNU GPL"
  :version "0.9"
  :depends-on ("split-sequence")
  :components ((:file "let-alist")))
