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


(defun add-permission (queue-url label permissions &key (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "AddPermission"
				:queue-url queue-url
				:parameters (acons "Label" label
						   (create-complex-n-member-parameters
						    (transform-parameters-plists '(:aws-account-id t :action-name t) permissions)
						    '("AWSAccountId." "ActionName."))))))
    (process-request sqs request)))

(defun change-message-visibility (queue-url receipt-handle visibility-timeout &key (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "ChangeMessageVisibility"
				:queue-url queue-url
				:parameters (alist-if-not-nil "VisibilityTimeout" visibility-timeout
							      "ReceiptHandle" receipt-handle))))
    (process-request sqs request)))

(defun change-message-visibility-batch (queue-url entries &key (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "ChangeMessageVisibilityBatch"
				:queue-url queue-url
				:parameters (create-complex-n-member-parameters
					     (transform-parameters-plists
					      '(:id t :receipt-handle t :visibility-timeout nil) entries)
					     "ChangeMessageVisibilityBatchRequestEntry."
					     '(".Id" ".ReceiptHandle" ".VisibilityTimeout" )))))
    (process-request sqs request)))

(defun create-queue (queue-name &key attributes (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "CreateQueue"
				:parameters (acons "QueueName" queue-name
						   (create-complex-n-member-parameters
						    (transform-parameters-plists '(:name t :value t) attributes)
						    "Attribute."
						    '(".Name" ".Value"))))))
    (process-request sqs request)))

(defun delete-message (queue-url receipt-handle &key (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "DeleteMessage"
				:queue-url queue-url
				:parameters (alist-if-not-nil "ReceiptHandle" receipt-handle))))
    (process-request sqs request)))

(defgeneric delete-message-batch (queue-url entries &key sqs)
  (:documentation "Deleting more than one message in request"))

(defmethod delete-message-batch (queue-url (delete-message-batch-action delete-message-batch-action) &key (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "DeleteMessageBatch"
				:queue-url queue-url
				:parameters (create-parameters delete-message-batch-action))))
    (process-request sqs request)))

(defmethod delete-message-batch (queue-url (entries list) &key (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "DeleteMessageBatch"
				:queue-url queue-url
				:parameters (create-complex-n-member-parameters
					     (transform-parameters-plists '(:id t :receipt-handle t) entries)
					     "DeleteMessageBatchRequestEntry."
					     '(".Id" ".ReceiptHandle")))))
    (process-request sqs request)))

(defun delete-queue (queue-url &key (sqs *sqs*))
  (let ((request (make-instance 'request
				:queue-url queue-url
				:action "DeleteQueue")))
    (process-request sqs request)))

(defun get-queue-attributes (queue-url attributes &key (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "GetQueueAttributes"
				:queue-url queue-url
				:parameters (list-to-indexed-parameters attributes
										"AttributeName."))))
    (process-request sqs request)))

(defun get-queue-url (queue-name &key (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "GetQueueUrl"
				:parameters `(("QueueName" . ,queue-name)))))
    (process-request sqs request)))

(defun list-dead-letter-source-queues (queue-url &key (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "ListDeadLetterSourceQueues"
				:queue-url queue-url)))
    (process-request sqs request)))

(defun list-queues (&key prefix (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "ListQueues"
				:parameters (alist-if-not-nil "QueueNamePrefix" prefix))))
    (process-request sqs request)))

(defun purge-queue (queue-url &key (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "PurgeQueue"
				:queue-url queue-url)))
    (process-request sqs request)))

(defun receive-message (queue-url &key max visibility-timeout wait-time attributes message-attributes (sqs *sqs*))
  (let* ((base-parameters (alist-if-not-nil "MaxNumberOfMessages" max
					    "VisibilityTimeout" visibility-timeout
					    "WaitTimeSeconds" wait-time))
	 (request (make-instance 'request
				 :action "ReceiveMessage"
				 :queue-url queue-url
				 :parameters (nconc
					      base-parameters
					      (list-to-indexed-parameters attributes "AttributeName.")
					      (list-to-indexed-parameters message-attributes "MessageAttributeName.")))))
    (process-request sqs request)))

(defun remove-permission (queue-url label &key (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "RemovePermission"
				:queue-url queue-url
				:parameters (alist-if-not-nil "Label" label))))
    (process-request sqs request)))


(defun send-message (queue-url message-body &key delay-seconds attributes (sqs *sqs*))
  (let* ((attributes-parameters (create-all-message-attributes-parameters attributes ""))
	 (parameters (alist-if-not-nil "MessageBody" message-body "DelaySeconds" delay-seconds))
	 (request (make-instance 'request
				 :action "SendMessage"
				 :queue-url queue-url
				 :parameters (nconc parameters attributes-parameters))))
    (process-request sqs request)))


(defgeneric send-message-batch (queue-url entries &key sqs)
  (:documentation "Sending more than one messsage in one request"))

(defmethod send-message-batch (queue-url (send-message-batch-action send-message-batch-action) &key (sqs *sqs*))
  (let* ((request (make-instance 'request
				 :action "SendMessageBatch"
				 :queue-url queue-url
				 :parameters (create-parameters send-message-batch-action))))
    (process-request sqs request)))

(defmethod send-message-batch (queue-url (entries list) &key (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "SendMessageBatch"
				:queue-url queue-url
				:parameters (create-send-message-batch-parameters entries))))
    (process-request sqs request)))


(defun set-queue-attributes (queue-url attributes &key (sqs *sqs*))
  (let ((request (make-instance 'request
				:action "SetQueueAttributes"
				:queue-url queue-url
				:parameters (create-complex-n-member-parameters
					     (transform-parameters-plists '(:name t :value t) attributes)
					     "Attribute."
					     '(".Name" ".Value")))))
    (process-request sqs request)))
