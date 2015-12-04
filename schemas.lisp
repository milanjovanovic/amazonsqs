;;;;
;;;; Copyright (c) Milan Jovanovic <milanj@gmail.com>
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;   * Redistributions of source code must retain the above copyright
;;;;     notice, this list of conditions and the following disclaimer.
;;;;
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;
;;;; schemas.lisp

(in-package #:amazonsqs)

(defparameter *schemas* (make-hash-table :test 'equal))

(defmacro add-response-schema (name &key (initial-fun nil) (return-fun nil) (start nil) (end nil))
  `(setf (gethash ,name *schemas*)
	 (vector
	  ,(or initial-fun '(constantly nil))
	  ,(or return-fun ''base-return-handler)
	  (list ,@(loop for (f l) in start
			collect `(list ,f ,l)))
	  (list ,@(loop for (f l) in end
			collect `(list ,f ,l))))))

(defun get-schema (response-name)
  (gethash response-name *schemas*))

(defun schema-initial-call (schema)
  (funcall (svref schema 0)))

(defun schema-return-call (schema value)
  (funcall (svref schema 1) value))

(defun schema-start-names (schema)
  (svref schema 2))

(defun schema-end-names (schema)
  (svref schema 3))

(defun schema-name-start-fun (schema field)
  (second (assoc field (schema-start-names schema) :test 'string-equal)))

(defun schema-name-end-fun (schema field)
  (second (assoc field (schema-end-names schema) :test 'string-equal)))

;;; handlers 

(defun make-multi-values-handler (mark)
  (lambda (value initial)
    (let* ((all-values (getf initial :value))
	   (current (first all-values)))
      (setf (getf initial :value)
	    (cons
	     (nconc current
		    (list (cons mark value)))
	     (cdr all-values)))
      initial)))

(defun multi-values-return-handler (value)
  (let ((first-value (reverse (getf value :value)))
	(second-value (getf value :second-value)))
    (values first-value second-value)))

(defun one-value-return-handler (value)
  (multiple-value-bind (base request-id) (multi-values-return-handler value)
    (values (first base) request-id)))

(defun multi-values-start-handler (value initial)
  (declare (ignore value))
  (let ((current (getf initial :value)))
    (setf (getf initial :value)
	  (cons nil current))
    initial))

(defun base-return-handler (value)
  (let* ((first-value (getf value :value))
	 (second-value (getf value :second-value)))
    (when (consp first-value)
      (setf first-value (reverse first-value)))
    (if second-value
	(values first-value second-value)
	(values first-value))))

(defun one-value-handler (value initial)
  (setf (getf initial :value) value)
  initial)

(defun values-list-handler (value initial)
  (let ((current-values (getf initial :value)))
    (setf (getf initial :value)
	  (cons value current-values))
    initial))

(defun second-value-handler (value initial)
  (setf (getf initial :second-value) value)
  initial)

;;; OBJECT HANDLER

(defun make-object-set-slot-handler (slot-name)
  (lambda (value initial)
    (let ((current-object (first (getf initial :value))))
      (setf (slot-value current-object slot-name)
	    value)
      initial)))

(defun make-object-allocate-handler (class)
  (lambda (value initial)
    (declare (ignore value))
    (setf (getf initial :value)
	  (cons (make-instance class)
		(getf initial :value)))
    initial))

(defun make-value-to-temp-plist-handler (mark)
  (lambda (value initial)
    (let ((temp (getf initial :temp)))
      (setf (getf initial :temp)
	    (nconc temp (list mark value))))
    initial))

(defun add-attributes (initial)
  (let* ((message (first (getf initial :value)))
	 (temp (getf initial :temp))
	 (attributes (attributes message)))
    (setf (attributes message)
	  (cons
	   (cons (getf temp :name)
		 (getf temp :value))
	   attributes))
    (setf (getf initial :temp) nil)
    initial))

(defun add-message-attributes (initial)
  (let* ((message (first (getf initial :value)))
	 (temp (getf initial :temp))
	 (message-attributes (message-attributes message)))
    (setf (message-attributes message)
	  (cons
	   (create-message-attribute temp)
	   message-attributes))
    (setf (getf initial :temp) nil)
    initial))

(defun create-message-attribute (attributes-alist)
  (let (value type name)
    (loop for (key avalue) on attributes-alist by #'cddr
	  do 
	     (case key
	       ((:binary-value :string-value) (setf value avalue))
	       (:name (setf name avalue))
	       (:data-type (cond ((string-equal avalue "String")
				  (setf type :string))
				 ((string-equal avalue "Binary")
				  (setf type :binary))
				 ((string-equal avalue "Number")
				  (setf type :number))
				 (t (error 'parsing-error :msg (format nil "Unknown message attribute type ~A" avalue)))))))
    (make-instance 'message-attribute :value value :type type :name name)))

(defun object-return-handler (value)
  (values
   (reverse (getf value :value))
   (getf value :second-value)))

(defun batch-results-return-handler (value)
  (let ((rvalue (getf value :value))
	(svalue (getf value :second-value)))
    (loop for object in rvalue
	  with successful = nil
	  with failed = nil
	  if (typep object 'batch-error-result)
	    do (push object failed)
	  else
	    do (push object successful)
	  end
	  finally (return (values
			   (make-instance 'batch-request-result
					  :successful successful
					  :failed failed)
			   svalue)))))

(defun error-object-return-handler (value)
  (flet ((alist-value (v mark)
	   (cdr (assoc mark v))))
    (let* ((error-alist (first (getf value :value)))
	   (type (alist-value error-alist :type))
	   (code (alist-value error-alist :code))
	   (message (alist-value error-alist :message))
	   (error-class (or (get-error-class code) 'sqs-native-error)))
      (error error-class :type type :code code :message message :msg "Got Error from Amazon SQS response"))))

;;;; SCHEMAS

(add-response-schema "AddPermissionResponse"
		     :start (("RequestId" 'one-value-handler)))

(add-response-schema "ChangeMessageVisibilityResponse"
		     :start (("RequestId" 'one-value-handler)))

(add-response-schema "ChangeMessageVisibilityBatchResponse"
		     :return-fun 'batch-results-return-handler
		     :start (("ChangeMessageVisibilityBatchResultEntry" (make-object-allocate-handler 'change-message-visibility-batch-result))
			     ("BatchResultErrorEntry" (make-object-allocate-handler 'batch-error-result))
			     ("Id" (make-object-set-slot-handler 'id))
			     ("Message" (make-object-set-slot-handler 'message))
			     ("SenderFault" (make-object-set-slot-handler 'sender-fault))
			     ("Code" (make-object-set-slot-handler 'code))
			     ("RequestId" 'second-value-handler)))

(add-response-schema "CreateQueueResponse"
		     :start (("QueueUrl" 'one-value-handler)
			     ("RequestId" 'second-value-handler)))

(add-response-schema "DeleteMessageResponse"
		     :start (("RequestId" 'one-value-handler)))


(add-response-schema "DeleteMessageBatchResponse"
		     :return-fun 'batch-results-return-handler
		     :start (("DeleteMessageBatchResultEntry" (make-object-allocate-handler 'delete-message-batch-result))
			     ("BatchResultErrorEntry" (make-object-allocate-handler 'batch-error-result))
			     ("Id" (make-object-set-slot-handler 'id))
			     ("Message" (make-object-set-slot-handler 'message))
			     ("SenderFault" (make-object-set-slot-handler 'sender-fault))
			     ("Code" (make-object-set-slot-handler 'code))
			     ("RequestId" 'second-value-handler)))

(add-response-schema "DeleteQueueResponse"
		     :start (("RequestId" 'one-value-handler)))

(add-response-schema "GetQueueAttributesResponse"
		     :return-fun (lambda (value)
				   (values (to-alist (reverse (getf value :value)))
					   (getf value :second-value)))
		     :start (("Name" 'values-list-handler)
			     ("Value" 'values-list-handler)
			     ("RequestId" 'second-value-handler)))

(add-response-schema "GetQueueUrlResponse"
		     :start (("QueueUrl" 'one-value-handler)
			     ("RequestId" 'second-value-handler)))

(add-response-schema "ListDeadLetterSourceQueuesResponse"
		     :start (("QueueUrl" 'values-list-handler)
			     ("RequestId" 'second-value-handler)))

(add-response-schema "ListQueuesResponse"
		     :start (("QueueUrl" 'values-list-handler)
			     ("RequestId" 'second-value-handler)))

(add-response-schema "PurgeQueueResponse"
		     :start (("RequestId" 'one-value-handler)))

(add-response-schema "ReceiveMessageResponse"
		     :return-fun 'object-return-handler
		     :start (("Message" (make-object-allocate-handler 'message))			     
			     ("MessageId" (make-object-set-slot-handler 'id))
			     ("ReceiptHandle" (make-object-set-slot-handler 'receipt-handle))
			     ("MD5OfBody" (make-object-set-slot-handler 'body-md5))
			     ("Body" (make-object-set-slot-handler 'body))
			     ("MD5OfMessageAttributes" (make-object-set-slot-handler 'attributes-md5))
			     ;; Attributes
			     ;; Name is mutual for Attributes and MessageAttributes
			     ("Name" (make-value-to-temp-plist-handler :name))
			     ("Value" (make-value-to-temp-plist-handler :value))
			     ;; MessageAttributes
			     ("DataType" (make-value-to-temp-plist-handler :data-type))
			     ("StringValue" (make-value-to-temp-plist-handler :string-value))
			     ("BinaryValue" (make-value-to-temp-plist-handler :binary-value))
			     ("RequestId" 'second-value-handler))
		     :end (("MessageAttribute" 'add-message-attributes)
			   ("Attribute" 'add-attributes)))

(add-response-schema "RemovePermissionResponse"
		     :start (("RequestId" 'one-value-handler)))

(add-response-schema "SendMessageResponse"
		     :return-fun 'one-value-return-handler
		     :start (("MD5OfMessageBody" (make-multi-values-handler :body-md5))
			     ("MD5OfMessageAttributes" (make-multi-values-handler :attributes-md5))
			     ("MessageId" (make-multi-values-handler :id))
			     ("RequestId" 'second-value-handler)))

(add-response-schema "SendMessageBatchResponse"
		     :return-fun 'batch-results-return-handler
		     :start (("SendMessageBatchResultEntry" (make-object-allocate-handler 'send-message-batch-result))
			     ("BatchResultErrorEntry" (make-object-allocate-handler 'batch-error-result))
			     ("Id" (make-object-set-slot-handler 'id))
			     ("MessageId" (make-object-set-slot-handler 'message-id))
			     ("MD5OfMessageBody" (make-object-set-slot-handler 'message-body-md5))
			     ("MD5OfMessageAttributes"(make-object-set-slot-handler 'message-attributes-md5))
			     ("Message" (make-object-set-slot-handler 'message))
			     ("SenderFault" (make-object-set-slot-handler 'sender-fault))
			     ("Code" (make-object-set-slot-handler 'code))
			     ("RequestId" 'second-value-handler)))

(add-response-schema "SetQueueAttributesResponse"
		     :start (("RequestId" 'one-value-handler)))

(add-response-schema "ErrorResponse"
		     :return-fun 'error-object-return-handler
		     :start (("Type"  (make-multi-values-handler :type))
			     ("Code"  (make-multi-values-handler :code))
			     ("Message"  (make-multi-values-handler :message))
			     ("RequestId" 'second-value-handler)))
