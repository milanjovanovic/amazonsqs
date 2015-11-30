(in-package :amazonsqs)

(defclass sqs-object ()
  ())

(defclass message (sqs-object)
  ((id :initarg :id
       :accessor id)
   (body :initarg :body
	 :accessor body)
   (receipt-handle :initarg :receipt-handle
		   :accessor receipt-handle)
   (body-md5 :initarg :body-md5
	     :accessor body-md5)
   (message-attributes :initarg :message-attributes
		       :accessor message-attributes
		       :initform nil)
   (attributes :initarg :attributes
	       :accessor attributes
	       :initform nil)
   (attributes-md5 :initarg :attributes-md5
		   :accessor attributes-md5
		   :initform nil)))

(defmethod print-object ((message message) stream)
  (print-unreadable-object (message stream :type t :identity t)
    (let* ((body-size (length (body message)))
	   (print-size (if (< body-size 5) body-size 5)))
      (princ (concatenate 'string
			  (subseq (body message) 0 print-size)
			  "...")
	     stream))))

(defclass batch-result (sqs-object)
  ((id :initarg :id :accessor id)))

(defmethod print-object ((batch-result batch-result) stream)
  (print-unreadable-object (batch-result stream :type t :identity t)
    (princ (id batch-result) 
	   stream)))

(defclass batch-result-error-entry (batch-result)
  ((code :initarg :code
	 :accessor code)
   (message :initarg :message
	    :accessor message
	    :initform nil)
   (sender-fault :initarg :sender-fault
		 :accessor sender-fault)))

(defclass delete-message-batch-entry (batch-result)
  ())

(defclass send-message-batch-entry (batch-result)
  ((message-id :initarg :message-id
	       :accessor message-id)
   (message-body-md5 :initarg :message-body-md5
		     :accessor message-body-md5)
   (message-attributes-md5 :initarg :message-attributes-md5
			   :accessor message-attributes-md5
			   :initform nil)))

(defclass change-message-visibility-batch-entry (batch-result)
  ())

(defclass batch-request-result (sqs-object)
  ((successful :initarg :successful
	       :accessor successful)
   (failed :initarg :failed
	   :accessor failed)))

(defmethod print-object ((result batch-request-result) stream)
  (print-unreadable-object (result stream :type t :identity t)
    (princ (format nil ":successful ~A, failed: ~A"
		   (length (successful result))
		   (length (failed result)))
	   stream)))

(defclass response ()
  ((status :accessor response-status :initarg :status)
   (request-id :accessor response-request-id :initarg :request-id)))

(defmethod print-object ((response response) stream)
  (print-unreadable-object (response stream :type t :identity nil)
    (princ (response-status response)
	   stream)))

;;; Objects for Batch SQS Calls ChangeMessageVisibilityBatch/DeleteMessageBatch/SendMessageBatch

(defun fill-indexes (entries)
  (loop for entry in entries
	for index from 1
	do (setf (index entry) index)))

(defclass batch-action (sqs-object)
  ())

(defclass batch-action-entry (sqs-object)
  ((index :initarg :index :accessor index :initform 1)))

(defgeneric create-parameters (batch-action-entry &optional parent)
  (:documentation "Creates parameters for sending in http request"))

(defgeneric base-name (batch-action-entry))

(defgeneric add-entry (parent child))

(defclass message-attribute (batch-action-entry)
  ((name :initarg :name :accessor message-attribute-name)
   (type :initarg :type :accessor message-attribute-type :initform :string)
   (value :initarg :value :accessor message-attribute-value)))

(defmethod base-name ((message-attribute message-attribute))
  "MessageAttribute")

(defun make-message-attribute (name type value)
  (make-instance 'message-attributes :name name :type type :value value))

(defun attribute-type-as-string (type)
  (ecase type
    (:string "String")
    (:binary "Binary")
    (:number "Number")))

(defun attribute-type-as-value-type-string (type)
  (ecase type
    (:string "Value.StringValue")
    (:binary "Value.BinaryValue")
    (:number "Value.StringValue")))

(defmethod create-parameters ((message-attribute message-attribute) &optional parent)
  (let ((parent-base-name (base-name parent))
	(parent-index (index parent))
	(attribute-base-name (base-name message-attribute))
	(attribute-index (index message-attribute)))
    (flet ((slot-api-name (field)
	     (format nil "~A.~A.~A.~A.~A" parent-base-name parent-index
		     attribute-base-name attribute-index field)))
      (alist-if-not-nil (slot-api-name "Name") (message-attribute-name message-attribute)
			(slot-api-name "Value.DataType") (attribute-type-as-string (message-attribute-type message-attribute))
			(slot-api-name (attribute-type-as-value-type-string 
					(message-attribute-type message-attribute))) (message-attribute-value message-attribute)))))

(defclass batch-message-entry (batch-action-entry)
  ((id :initarg :id :accessor id)
   (body :initarg :body :accessor body)
   (delay :initarg :delay :accessor delay :initform nil)
   (attributes :initarg :attributes :accessor attributes :initform nil)))

(defmethod base-name ((batch-message-entry batch-message-entry))
  "SendMessageBatchRequestEntry")

(defmethod create-parameters ((batch-message-entry batch-message-entry) &optional parent)
  (declare (ignore parent))
  (let ((attributes (reverse (copy-list (attributes batch-message-entry)))))
    ;; fill attributes index field so we can create right ordered parameters
    (fill-indexes attributes)
    (let* ((index (index batch-message-entry))
	   (base-name (base-name batch-message-entry))
	   (base (format nil "~A.~A" base-name index)))
      (flet ((slot-api-name (field)
	       (format nil "~A.~A" base field)))
	(let ((base-parameters (alist-if-not-nil (slot-api-name "Id") (id batch-message-entry)
						 (slot-api-name "MessageBody") (body batch-message-entry)
						 (slot-api-name "DelaySeconds") (delay batch-message-entry)))
	      (attributes-parameters (mapcan (lambda (attribute)
					       (create-parameters attribute batch-message-entry))
					     attributes)))
	  (nconc base-parameters attributes-parameters))))))

(defmethod add-entry ((parent batch-message-entry) (child message-attribute))
  (setf (attributes parent)
	(cons child
	      (attributes parent))))

(defclass send-message-batch-action (batch-action)
  ((messages :initarg :messages :accessor messages :initform nil)))

(defmethod add-entry ((parent send-message-batch-action) (child batch-message-entry))
  (setf (messages parent)
	(cons child
	      (messages parent))))

(defmethod create-parameters ((batch-action send-message-batch-action) &optional parent)
  (declare (ignore parent))
  (let ((messages (reverse (copy-list (messages batch-action)))))
    (fill-indexes messages)
    (mapcan (lambda (message)
	      (create-parameters message batch-action))
	    messages)))
