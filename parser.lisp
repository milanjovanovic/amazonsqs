(in-package :amazonsqs)

(defun parse-sqs-response (source schema &optional initial-value start-element-type)
  (let ((initial-value (or initial-value (schema-initial-call schema)))
	(type (or start-element-type
		  (klacks:peek-next source))))
    (cond ((eq type :START-ELEMENT)
	   (let* ((name (klacks:current-lname source))
		  (callback (schema-name-start-fun schema name)))
	     (if callback
		 (parse-sqs-response source schema (funcall callback (get-next-value source) initial-value) (klacks:peek source))
		 (parse-sqs-response source schema initial-value))))
	  ((eq type :END-ELEMENT)
	   (let* ((name (klacks:current-lname source))
		  (callback (schema-name-end-fun schema name)))
	     (if callback
		 (parse-sqs-response source schema (funcall callback initial-value))
		 (parse-sqs-response source schema initial-value))))
	  ((eq type :END-DOCUMENT)
	   (schema-return-call schema initial-value))
	  (t
	   (parse-sqs-response source schema initial-value)))))

(defun create-response (source)
  (let ((type (klacks:peek source)))
    (cond ((eq type :START-ELEMENT)
	   (let* ((lname (klacks:current-lname source))
		  (schema (get-schema lname)))
	     (unless schema
	       (error 'parsing-error :msg (format nil "Unknown schema for response: ~A" lname)))
	     (parse-sqs-response source schema nil type)))
	  ((eq type :END-DOCUMENT) 
	   (error 'parsing-error :msg "Unknown response schema !!!"))
	  (t
	   (progn
	     (klacks:peek-next source)
	     (create-response source))))))

(defun get-next-value (source)
  (let ((type (klacks:peek-next source)))
    (if (eq type :CHARACTERS)
	(remove-blanks (klacks:current-characters source))
	nil)))
