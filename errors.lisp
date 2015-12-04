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
;;;; errors.lisp

(in-package #:amazonsqs)

(defparameter *conditions* (make-hash-table :test 'equalp))

(define-condition sqs-error (error)
  ((msg :initarg :msg :reader msg))
  (:report (lambda (condition stream)
	     (format stream "~A" (msg condition)))))

(define-condition parsing-error (sqs-error)
  ())

(define-condition bad-parameters (sqs-error)
  ())

(define-condition sqs-native-error (sqs-error)
  ((type :initarg :type :reader sqs-error-type)
   (code :initarg :code :reader sqs-error-code)
   (message :initarg :message :reader sqs-error-message))
  (:report (lambda (condition stream)
	     (format stream "~A, Message: ~A, Code: ~A, Type: ~A" (msg condition)
		     (sqs-error-message condition)
		     (sqs-error-code condition)
		     (sqs-error-type condition)))))

(defmacro define-amazon-condition (lisp-name amazon-name &optional slots)
  `(progn
     (define-condition ,lisp-name (sqs-native-error)
       ,(append (list) slots))
     (setf (gethash ,amazon-name *conditions*) ',lisp-name)))

(defun get-error-class (amazon-name)
  (gethash amazon-name *conditions*))


;;; AMAZON SQS ERRORS
;;; common errors
(define-amazon-condition incomplete-signature "IncompleteSignature")
(define-amazon-condition internal-failure "InternalFailure")
(define-amazon-condition invalid-action "InvalidAction")
(define-amazon-condition invalid-client-tokenid "InvalidClientTokenId")
(define-amazon-condition invalid-parameter-combination "InvalidParameterCombination")
(define-amazon-condition invalid-parameter-value "InvalidParameterValue")
(define-amazon-condition invalid-query-parameter "InvalidQueryParameter")
(define-amazon-condition malformed-querys-tring "MalformedQueryString")
(define-amazon-condition missing-action "MissingAction")
(define-amazon-condition missing-authentication-token "MissingAuthenticationToken")
(define-amazon-condition missing-parameter "MissingParameter")
(define-amazon-condition opt-in-required "OptInRequired")
(define-amazon-condition request-Expired "RequestExpired")
(define-amazon-condition service-Unavailable "ServiceUnavailable")
(define-amazon-condition throttling "Throttling")
(define-amazon-condition validation-error "ValidationError")

;;; actions specific errors
(define-amazon-condition batch-entry-ids-not-distinct "BatchEntryIdsNotDistinct")
(define-amazon-condition batch-request-too-long "BatchRequestTooLong")
(define-amazon-condition empty-batch-request "EmptyBatchRequest")
(define-amazon-condition invalid-attribute-name "InvalidAttributeName")
(define-amazon-condition invalid-batch-entry-id "InvalidBatchEntryId")
(define-amazon-condition invalid-id-format "InvalidIdFormat")
(define-amazon-condition invalid-message-contents "InvalidMessageContents")
(define-amazon-condition message-not-inflight "MessageNotInflight")
(define-amazon-condition over-limit "OverLimit")
(define-amazon-condition purge-queue-in-progress "PurgeQueueInProgress")
(define-amazon-condition queue-deleted-recently "QueueDeletedRecently")
(define-amazon-condition queue-does-not-exist "QueueDoesNotExist")
(define-amazon-condition queue-name-exists "QueueNameExists")
(define-amazon-condition receipt-handle-is-invalid "ReceiptHandleIsInvalid")
(define-amazon-condition too-many-entries-in-batch-request "TooManyEntriesInBatchRequest")
(define-amazon-condition unsupported-operation "UnsupportedOperation")


;;; missing
(define-amazon-condition signature-does-not-match "SignatureDoesNotMatch")
(define-amazon-condition non-existent-queue "AWS.SimpleQueueService.NonExistentQueue")

