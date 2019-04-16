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
;;;; objects.lisp

(in-package #:amazonsqs)

(defclass sqs-object ()
  ())

(defclass message (sqs-object)
  ((id :initarg :id
       :accessor message-id)
   (body :initarg :body
	 :accessor message-body)
   (receipt-handle :initarg :receipt-handle
		   :accessor message-receipt-handle)
   (body-md5 :initarg :body-md5
	     :accessor message-body-md5)
   (attributes :initarg :message-attributes
		       :accessor message-attributes
		       :initform nil)
   (base-attributes :initarg :attributes
	       :accessor message-base-attributes
	       :initform nil)
   (attributes-md5 :initarg :attributes-md5
		   :accessor message-attributes-md5
		   :initform nil)))

(defmethod print-object ((message message) stream)
  (print-unreadable-object (message stream :type t :identity t)

    (let* ((body-size (length (message-body message)))
	   (print-size (if (< body-size 5) body-size 5)))
      (princ (concatenate 'string
			  (subseq (message-body message) 0 print-size)
			  "...")
	     stream))))

(defclass batch-result (sqs-object)
  ((id :initarg :id :accessor id)))

(defmethod print-object ((batch-result batch-result) stream)
  (print-unreadable-object (batch-result stream :type t :identity nil)
    (princ (id batch-result) 
	   stream)))

(defclass batch-error-result (batch-result)
  ((code :initarg :code
	 :accessor batch-error-code)
   (message :initarg :message
	    :accessor batch-error-message
	    :initform nil)
   (sender-fault :initarg :sender-fault
		 :accessor batch-error-sender-fault)))

(defclass delete-message-batch-result (batch-result)
  ())

(defclass send-message-batch-result (batch-result)
  ((message-id :initarg :message-id
	       :accessor message-id)
   (message-body-md5 :initarg :message-body-md5
		     :accessor message-body-md5)
   (message-attributes-md5 :initarg :message-attributes-md5
			   :accessor message-attributes-md5
			   :initform nil)))

(defclass change-message-visibility-batch-result (batch-result)
  ())

(defclass batch-request-result (sqs-object)
  ((successful :initarg :successful
	       :accessor batch-successful)
   (failed :initarg :failed
	   :accessor batch-failed)))

(defmethod print-object ((result batch-request-result) stream)
  (print-unreadable-object (result stream :type t :identity t)
    (princ (format nil ":successful ~A, failed: ~A"
		   (length (batch-successful result))
		   (length (batch-failed result)))
	   stream)))

(defclass response ()
  ((status :accessor response-status :initarg :status)
   (request-id :accessor response-request-id :initarg :request-id)))

(defmethod print-object ((response response) stream)
  (print-unreadable-object (response stream :type t :identity nil)
    (princ (response-status response)
	   stream)))

;;; Objects for Batch SQS Calls ChangeMessageVisibilityBatch/DeleteMessageBatch/SendMessageBatch

(defclass batch-action (sqs-object)
  ())

(defgeneric create-parameters (batch-action-entry &optional parent index parent-index)
  (:documentation "Creates parameters for sending in http request"))

(defgeneric base-name (batch-action-entry))

(defclass message-attribute ()
  ((name :initarg :name :accessor message-attribute-name)
   (type :initarg :type :accessor message-attribute-type :initform :string)
   (value :initarg :value :accessor message-attribute-value)))

(defmethod print-object ((message-attribute message-attribute) stream)
  (print-unreadable-object (message-attribute stream :type t :identity nil)
    (princ (message-attribute-name message-attribute)
	   stream)))

(defmethod base-name ((message-attribute message-attribute))
  "MessageAttribute")

(defun make-message-attribute (name type value)
  (make-instance 'message-attribute :name name :type type :value value))

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

(defmethod create-parameters ((message-attribute message-attribute) &optional parent index parent-index)
  (let ((parent-base-name (base-name parent))
	(attribute-base-name (base-name message-attribute)))
    (flet ((slot-api-name (field)
	     (format nil "~A.~A.~A.~A.~A" parent-base-name parent-index
		     attribute-base-name index field)))
      (alist-if-not-nil (slot-api-name "Name") (message-attribute-name message-attribute)
			(slot-api-name "Value.DataType") (attribute-type-as-string (message-attribute-type message-attribute))
			(slot-api-name (attribute-type-as-value-type-string 
					(message-attribute-type message-attribute))) (message-attribute-value message-attribute)))))

(defclass batch-message-entry ()
  ((id :initarg :id :accessor id)
   (body :initarg :body :accessor message-body)
   (delay :initarg :delay :accessor message-delay :initform nil)
   (attributes :initarg :attributes :accessor message-attributes :initform nil)
   (message-dedup-id :initarg :message-dedup-id :accessor message-dedup-id :initform nil)
   (message-group-id :initarg :message-group-id :accessor message-group-id :initform nil)))

(defmethod base-name ((batch-message-entry batch-message-entry))
  "SendMessageBatchRequestEntry")

(defmethod create-parameters ((batch-message-entry batch-message-entry) &optional parent index parent-index)
  (declare (ignore parent parent-index))
  (multiple-value-bind (attributes attributes-count) (reverse-and-count (message-attributes batch-message-entry))
    (let ((base (format nil "~A.~A" (base-name batch-message-entry) index)))
      (flet ((slot-api-name (field)
	       (format nil "~A.~A" base field)))
	(let ((base-parameters (alist-if-not-nil (slot-api-name "Id") (id batch-message-entry)
						 (slot-api-name "MessageBody") (message-body batch-message-entry)
						 (slot-api-name "DelaySeconds") (message-delay batch-message-entry)
						 (slot-api-name "MessageDeduplicationId") (message-dedup-id batch-message-entry)
						 (slot-api-name "MessageGroupId") (message-group-id batch-message-entry)))
	      (attributes-parameters (mapcan (lambda (attribute attribute-index)
					       (create-parameters attribute batch-message-entry attribute-index index))
					     attributes
					     (alexandria:iota attributes-count :start 1))))
	  (nconc base-parameters attributes-parameters))))))

(defun add-message-attribute-entry (batch-message-entry message-attribute)
  (setf (message-attributes batch-message-entry)
	(cons message-attribute
	      (message-attributes batch-message-entry)))
  batch-message-entry)

(defclass send-message-batch-action (batch-action)
  ((messages :initarg :messages :accessor messages :initform nil)))

(defun add-message-entry (action message-entry)
  (setf (messages action)
	(cons message-entry
	      (messages action)))
  action)

(defclass delete-message-batch-action (batch-action)
  ((messages :initarg :messages :accessor messages :initform nil)))

(defclass batch-message-delete-entry ()
  ((id :initarg :id :accessor id)
   (receipt-handle :initarg :receipt-handle :accessor message-receipt-handle)))

(defmethod base-name ((batch-message-delete-entry batch-message-delete-entry))
  "DeleteMessageBatchRequestEntry")

(defmethod create-parameters ((message-entry batch-message-delete-entry) &optional parent index parent-index)
  (declare (ignore parent parent-index))
  (let* ((base (format nil "~A.~A" (base-name message-entry) index)))
    (flet ((slot-api-name (field)
	     (format nil "~A.~A" base field)))
      (alist-if-not-nil (slot-api-name "Id") (id message-entry)
			(slot-api-name "ReceiptHandle") (message-receipt-handle message-entry)))))


(defmethod create-parameters ((batch-action batch-action) &optional parent index parent-index)
  (declare (ignore parent index parent-index))
  (multiple-value-bind (messages messages-count) (reverse-and-count (messages batch-action))
    (mapcan (lambda (message message-index)
	      (create-parameters message batch-action message-index))
	    messages
	    (alexandria:iota messages-count :start 1))))
