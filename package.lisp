;;;; package.lisp

(defpackage #:amazonsqs
  (:use #:cl)
  (:export
   #:*sqs*
   ;; objects
   #:awscredentials
   #:sqs
   #:message
   #:batch-result-error-entry
   #:delete-message-batch-entry
   #:send-message-batch-entry
   #:change-message-visibility-batch-entry
   #:batch-request-result
   #:response ; check this
   #:message-attribute ; change this
   #:batch-message-entry
   #:send-message-batch-action
   #:add-entry
   ;; Amazon API calls
   #:load-aws-credentials
   #:add-permission
   #:change-message-visibility
   #:change-message-visibility-batch
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
   ;; conditions
   ))
