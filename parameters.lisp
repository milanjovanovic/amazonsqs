(in-package :amazonsqs)

(defmethod make-parameters-combo ((value list) index base-names suffixes)
  (mapcan (lambda (v base-name suffix)
	    (make-parameters-combo v index base-name suffix))
	  value base-names suffixes))

(defmethod make-parameters-combo ((value t) index base-name suffix)
  (when value
    (list (cons (concatenate 'string  base-name (write-to-string index) suffix)
		value))))

(defun list-to-indexed-parameters (values base-name &optional suffix)
  (let ((suffix (or suffix "")))
    (mapcan (lambda (value index)
	      (make-parameters-combo value index base-name suffix))
	    values (alexandria:iota (length values) :start 1))))

(defun transform-parameters-plists (mark params)
  (mapcar (lambda (param)
	    (parse-parameter-plist mark param))
	  params))

(defun parse-parameter-plist (mark param)
  (loop for (mark needs) on mark by #'cddr
	for value = (getf param mark)
	for result = (if value
			 value
			 (if needs
			     (error 'bad-parameters :msg (format nil "Missing parameter ~A in ~A" mark param))
			     nil))
	collecting result))

(defun create-complex-n-member-parameters (complex-values base-names &optional suffixes)
  (let* ((elem-length (length (first complex-values)))
	 (suffixes (or suffixes (make-list elem-length :initial-element "")))
	 (base-names (if (listp base-names) base-names (make-list elem-length :initial-element base-names))))
    (mapcan (lambda (one-value index)
	      (make-parameters-combo one-value index base-names suffixes))
	    complex-values
	    (alexandria:iota (length complex-values) :start 1))))


(defun create-message-attribute-parameters (prefix index plist)
  (flet ((parameter-name (field)
	   (format nil "~A~A.~A.~A" prefix
		   "MessageAttribute" index field)))
    (let (aname avalue atype avalue-name)
      (loop for (key value) on plist by #'cddr
	    do
	       (case key
		 (:name (setf aname value))
		 (:string-value
		  (setf avalue value)
		  (setf avalue-name "Value.StringValue"))
		 (:binary-value
		  (setf avalue value)
		  (setf avalue-name "Value.BinaryValue"))
		 (:number-value
		  (setf avalue value)
		  (setf avalue-name "Value.StringValue"))
		 (:data-type (setf atype value))
		 (otherwise)))
      (unless (and aname avalue atype avalue-name)
	(error 'bad-parameters :msg (format nil "Bad arguments ~A" plist)))
      (list (cons (parameter-name "Name") aname)
	    (cons (parameter-name avalue-name) avalue)
	    (cons (parameter-name "Value.DataType") atype)))))

(defun create-all-message-attributes-parameters (plist prefix)
  (mapcan
   (lambda (attr-plist attribute-index)
     (create-message-attribute-parameters prefix
					  attribute-index
					  attr-plist))
   plist
   (alexandria:iota (length plist) :start 1)))

(defun create-message-batch-entry-parameters (plist index)
  (let (id body delay-seconds attributes-parameters)
    (loop for (key value) on plist by #'cddr
	  do
	     (ecase key
	       (:id (setf id value))
	       (:body (setf body value))
	       (:delay-seconds (setf delay-seconds value))
	       (:attributes (setf attributes-parameters (create-all-message-attributes-parameters
							 value
							 (format nil "SendMessageBatchRequestEntry.~A." index))))
	       (otherwise)))
    (unless (and id body)
      (error 'bad-parameters :msg (format nil "Bad arguments ~A" plist)))
    (nconc (alist-if-not-nil (format nil "SendMessageBatchRequestEntry.~A.Id" index) id
			     (format nil "SendMessageBatchRequestEntry.~A.MessageBody" index) body
			     (format nil "SendMessageBatchRequestEntry.~A.DelaySeconds" index) delay-seconds)
	   attributes-parameters)))

(defun create-send-message-batch-parameters (plists)
  (mapcan (lambda (plist index)
	    (create-message-batch-entry-parameters plist index))
	  plists
	  (alexandria:iota (length plists) :start 1)))
