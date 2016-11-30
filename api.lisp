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
;;;; api.lisp

(in-package #:amazonsqs)

(defparameter *sqs* nil)

(defun load-aws-credentials (file)
  (with-open-file (stream file)
    (make-instance 'awscredentials
		   :access-key (read-line stream)
		   :secret-key (read-line stream))))

(defun make-aws-credentials (access-key secret-key)
  (make-instance 'awscredentials :access-key access-key :secret-key secret-key))


;;; idea stolen from zs3 library
(defmacro with-cached-stream (&body body)
  "Reusing thread-local cached connection"
  `(let ((*cached-stream* nil)
	 (*do-cache-stream* t))
     (unwind-protect
	  (progn ,@body)
       (when *cached-stream*
         (ignore-errors (close *cached-stream*))))))


(defun add-permission (queue-url label permissions &key (sqs *sqs*))
  (let ((request (make-request "AddPermission"
			       (acons "Label" label
				      (create-complex-n-member-parameters
				       (transform-parameters-plists '(:aws-account-id t :action-name t) permissions)
				       '("AWSAccountId." "ActionName.")))
			       queue-url)))
    (process-request sqs request)))

(defun change-message-visibility (queue-url receipt-handle visibility-timeout &key (sqs *sqs*))
  (let ((request (make-request "ChangeMessageVisibility"
			       (alist-if-not-nil "VisibilityTimeout" visibility-timeout
						 "ReceiptHandle" receipt-handle)
			       queue-url)))
    (process-request sqs request)))

(defun change-message-visibility-batch (queue-url entries &key (sqs *sqs*))
  (let ((request (make-request "ChangeMessageVisibilityBatch"
			       (create-complex-n-member-parameters
				(transform-parameters-plists
				 '(:id t :receipt-handle t :visibility-timeout nil) entries)
				"ChangeMessageVisibilityBatchRequestEntry."
				'(".Id" ".ReceiptHandle" ".VisibilityTimeout" ))
			       queue-url)))
    (process-request sqs request)))

(defun create-queue (queue-name &key attributes (sqs *sqs*))
  (let ((request (make-request "CreateQueue"
			       (acons "QueueName" queue-name
				      (create-complex-n-member-parameters
				       (transform-parameters-plists '(:name t :value t) attributes)
				       "Attribute."
				       '(".Name" ".Value"))))))
    (process-request sqs request)))

(defun delete-message (queue-url receipt-handle &key (sqs *sqs*))
  (let ((request (make-request "DeleteMessage"
			       (alist-if-not-nil "ReceiptHandle" receipt-handle)
			       queue-url)))
    (process-request sqs request)))

(defgeneric delete-message-batch (queue-url entries &key sqs)
  (:documentation "Deleting more than one message in request"))

(defmethod delete-message-batch (queue-url (delete-message-batch-action delete-message-batch-action) &key (sqs *sqs*))
  (let ((request (make-request "DeleteMessageBatch"
			       (create-parameters delete-message-batch-action)
			       queue-url)))
    (process-request sqs request)))

(defmethod delete-message-batch (queue-url (entries list) &key (sqs *sqs*))
  (let ((request (make-request "DeleteMessageBatch"
			       (create-complex-n-member-parameters
				(transform-parameters-plists '(:id t :receipt-handle t) entries)
				"DeleteMessageBatchRequestEntry."
				'(".Id" ".ReceiptHandle"))
			       queue-url)))
    (process-request sqs request)))

(defun delete-queue (queue-url &key (sqs *sqs*))
  (let ((request (make-request "DeleteQueue" nil queue-url)))
    (process-request sqs request)))

(defun get-queue-attributes (queue-url attributes &key (sqs *sqs*))
  (let ((request (make-request "GetQueueAttributes"
			       (list-to-indexed-parameters attributes "AttributeName.")
			       queue-url)))
    (process-request sqs request)))

(defun get-queue-url (queue-name &key (sqs *sqs*))
  (let ((request (make-request "GetQueueUrl" `(("QueueName" . ,queue-name)))))
    (process-request sqs request)))

(defun list-dead-letter-source-queues (queue-url &key (sqs *sqs*))
  (let ((request (make-request "ListDeadLetterSourceQueues" nil queue-url)))
    (process-request sqs request)))

(defun list-queues (&key prefix (sqs *sqs*))
  (let ((request (make-request "ListQueues"
			       (alist-if-not-nil "QueueNamePrefix" prefix))))
    (process-request sqs request)))

(defun purge-queue (queue-url &key (sqs *sqs*))
  (let ((request (make-request "PurgeQueue" nil queue-url)))
    (process-request sqs request)))

(defun receive-message (queue-url &key max visibility-timeout wait-time
				       base-attributes message-attributes receive-request-attempt-id (sqs *sqs*))
  (let* ((base-parameters (alist-if-not-nil "MaxNumberOfMessages" max
					    "VisibilityTimeout" visibility-timeout
					    "WaitTimeSeconds" wait-time
					    "ReceiveRequestAttemptId" receive-request-attempt-id))
	 (request (make-request "ReceiveMessage"
				(nconc
				 base-parameters
				 (list-to-indexed-parameters base-attributes "AttributeName.")
				 (list-to-indexed-parameters message-attributes "MessageAttributeName."))
				queue-url)))
    (process-request sqs request)))

(defun remove-permission (queue-url label &key (sqs *sqs*))
  (let ((request (make-request "RemovePermission"
			       (alist-if-not-nil "Label" label)
			       queue-url)))
    (process-request sqs request)))

(defun send-message (queue-url message-body
		     &key delay-seconds attributes message-dedup-id message-group-id (sqs *sqs*))
  (let* ((attributes-parameters (create-all-message-attributes-parameters attributes ""))
	 (parameters (alist-if-not-nil "MessageBody" message-body
				       "DelaySeconds" delay-seconds
				       "MessageDeduplicationId" message-dedup-id
				       "MessageGroupId" message-group-id))
	 (request (make-request "SendMessage"
				(nconc parameters attributes-parameters)
				queue-url)))
    (process-request sqs request)))

(defgeneric send-message-batch (queue-url entries &key sqs)
  (:documentation "Sending more than one messsage in one request"))

(defmethod send-message-batch (queue-url (send-message-batch-action send-message-batch-action) &key (sqs *sqs*))
  (let* ((request (make-request "SendMessageBatch"
				(create-parameters send-message-batch-action)
				queue-url)))
    (process-request sqs request)))

(defmethod send-message-batch (queue-url (entries list) &key (sqs *sqs*))
  (let ((request (make-request "SendMessageBatch"
			       (create-send-message-batch-parameters entries)
			       queue-url)))
    (process-request sqs request)))

(defun set-queue-attributes (queue-url attributes &key (sqs *sqs*))
  (let ((request (make-request "SetQueueAttributes"
			       (create-complex-n-member-parameters
				(transform-parameters-plists '(:name t :value t) attributes)
				"Attribute."
				'(".Name" ".Value"))
			       queue-url)))
    (process-request sqs request)))
