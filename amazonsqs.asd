;;;; amazonsqs.asd

(asdf:defsystem #:amazonsqs
  :description "Amazon Simple Queue Service CL client"
  :author "Milan Jovanovic <milanj@gmail.com>"
  :license "BSD"
  :serial t
  :depends-on (#:drakma #:cxml #:alexandria #:ironclad #:bordeaux-threads)
  :components ((:file "package")
	       (:file "amazonsqs")
	       (:file "utils")
	       (:file "errors")
	       (:file "objects")
	       (:file "schemas")
	       (:file "parser")
	       (:file "request")
	       (:file "parameters")
	       (:file "api")))

