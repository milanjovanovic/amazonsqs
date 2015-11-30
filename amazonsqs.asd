;;;; amazonsqs.asd

(asdf:defsystem #:amazonsqs
  :description "Amazon Simple Queue Service CL client"
  :author "Milan Jovanovic <your.name@example.com>"
  :license "Specify license here"
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

