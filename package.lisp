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
;;;; package.lisp

(defpackage #:amazonsqs
  (:use #:cl)
  (:export
   #:*sqs*
   #:awscredentials
   #:make-aws-credentials
   #:sqs
   #:connection-pooling-sqs
   #:with-cached-stream
   #:*do-cache-stream*
   #:*cached-stream*
   #:close-sqs
   #:response
   #:response-status
   #:response-request-id
   ;; message
   #:message
   #:message-id
   #:message-body
   #:message-body-md5
   #:message-receipt-handle
   #:message-base-attributes
   #:message-attributes
   #:message-attributes-md5
   ;; batch entries
   #:send-message-batch-action
   #:delete-message-batch-action
   #:messages
   #:add-message-entry
   #:add-message-attributes-entry
   #:id
   ;; message entry in send message batch request
   #:batch-message-entry
   ;; message entry in delete message batch request
   #:batch-message-delete-entry
   ;; message attributes in batch request
   #:message-attribute
   #:make-message-attribute
   #:message-attribute-name
   #:message-attribute-type
   #:message-attribute-value
   ;; batch results
   #:batch-request-result
   #:batch-successful
   #:batch-failed
   ;; batch error entry
   #:batch-error-result
   #:batch-error-code
   #:batch-error-message
   #:batch-error-sender-fault
   ;; batch results
   #:delete-message-batch-result
   #:send-message-batch-result
   #:make-visibility
   #:change-message-visibility-batch-result
   ;; Amazon API calls
   #:load-aws-credentials
   #:make-permission
   #:add-permission
   #:change-message-visibility
   #:change-message-visibility-batch
   #:make-queue-attribute
   #:create-queue
   #:delete-message
   #:delete-message-batch
   #:delete-queue
   #:get-queue-attributes
   #:get-queue-url
   #:list-dead-letter-source-queues
   #:list-queues
   #:purge-queue
   #:receive-message
   #:remove-permission
   #:send-message
   #:send-message-batch
   #:set-queue-attributes
   ;; FIXME, export conditions
   ))
