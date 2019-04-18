;;;; amazonsqs.asd

(asdf:defsystem #:amazonsqs-tests
  :description "Testing"
  :author "Milan Jovanovic <milanj@gmail.com>"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:amazonsqs #:lisp-unit)
  :components ((:file "tests")))
