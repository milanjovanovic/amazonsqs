(defpackage #:amazonsqs.tests
  (:use #:cl #:amazonsqs :lisp-unit))

(in-package #:amazonsqs.tests)

(defparameter *simple-sqs* nil)
(defparameter *pooling-sqs* nil)

(defparameter *queue-name* nil)

(defun run-all-tests (creds)
  (let ((*simple-sqs* (make-instance 'sqs :aws-credentials creds))
	(*pooling-sqs* (make-instance 'connection-pooling-sqs :aws-credentials creds :pool-size 5))
	(*print-failures* t)
	(*print-errors* t)
	(*print-summary* t)
	(*queue-name* (format nil "~A~A" "amazonsqsTestQueue" (random 10000000))))
    (run-tests-for-client *simple-sqs*)
    (run-tests-for-client *pooling-sqs*)))

(defun run-tests-for-client (client)
  (let ((*queue-name* (format nil "~A~A" "amazonsqsTestQueue" (random 100000000))))
    (format t "Test client ~A, queue: ~A~%" client *queue-name*)
    (let* ((*sqs* client)
	   (summary (run-tests '(queue-create queue-attributes send-delete-msg-1 send-batch-1 receive-delete-1 queue-delete))))
      summary)))

(defun run-parallel-tests (client)
  (declare (ignore client)))

(defun get-attribute-value (attributes attribute-name)
  (cdr (assoc attribute-name attributes :test 'equal)))

(defun queue-url-p (queue-name queue-url)
  (and (typep queue-url 'string)
       (search "http://" queue-url)
       (search queue-name queue-url)))

(define-test queue-create
  (:tag :sqs :single-thread :queue)
  (multiple-value-bind (queues response) (list-queues :prefix *queue-name*)
    (assert-nil queues)
    (assert-numerical-equal 200 (response-status response)))
  (assert-error 'AMAZONSQS::NON-EXISTENT-QUEUE (get-queue-url *queue-name*))
  (multiple-value-bind (queue-url create-queue-response) (create-queue *queue-name* :attributes
								       (list
									(make-queue-attribute "DelaySeconds" 0)
									(make-queue-attribute "ReceiveMessageWaitTimeSeconds" 1)
									(make-queue-attribute "VisibilityTimeout" 300)))
    (assert-true (queue-url-p *queue-name* queue-url))
    (assert-numerical-equal 200 (response-status create-queue-response))
    (assert-equal queue-url (get-queue-url *queue-name*)))
  (assert-error 'AMAZONSQS::INVALID-PARAMETER-VALUE (create-queue "not-legal-name.")))

(define-test queue-attributes
  (:tag :sqs :single-thread :queue)
  (multiple-value-bind (queue-url response) (get-queue-url *queue-name*) 
    (assert-true (queue-url-p *queue-name* queue-url))
    (assert-numerical-equal 200 (response-status response))
    (assert-equal queue-url (get-queue-url *queue-name*))
    (multiple-value-bind (attributes response-attr) (get-queue-attributes queue-url '("DelaySeconds" "VisibilityTimeout"))
      (assert-numerical-equal 200 (response-status response-attr))
      (assert-numerical-equal 2 (length attributes))
      (assert-numerical-equal 0 (parse-integer (get-attribute-value attributes "DelaySeconds")))
      (assert-numerical-equal 300 (parse-integer (get-attribute-value attributes "VisibilityTimeout"))))
    (let ((set-attr-response (set-queue-attributes queue-url (list
							      (make-queue-attribute "VisibilityTimeout" 360)
							      (make-queue-attribute "DelaySeconds" 1)))))
      (assert-numerical-equal 200 (response-status set-attr-response))
      (sleep 3)
      (let ((new-attributes (get-queue-attributes queue-url '("VisibilityTimeout" "DelaySeconds"))))
	(assert-numerical-equal 360 (parse-integer (get-attribute-value new-attributes "VisibilityTimeout")))
	(assert-numerical-equal 1 (parse-integer (get-attribute-value new-attributes "DelaySeconds")))))))

(define-test queue-delete
  (:tag :sqs :single-thread :queue)
  (assert-equal 1 (length (list-queues :prefix (subseq *queue-name* 0 (- (length *queue-name*) 2 )))))
  (let ((response (delete-queue (get-queue-url *queue-name*))))
    (assert-numerical-equal 200 (response-status response))))

(define-test send-delete-msg-1
  (:tag :sqs :single-thread :message)
  (let ((queue-url (get-queue-url *queue-name*)))
    (send-message queue-url "msg body" :delay-seconds 3 :attributes (list
								     (make-message-attribute "Attribute-1"
											     :string "string attributes")
								     (make-message-attribute "Attribute-2"
											     :number 1)))
    (multiple-value-bind (messages-1 receive-response-1) (receive-message queue-url)
      (assert-nil messages-1)
      (assert-equal 200 (response-status receive-response-1)))
    (sleep 5)
    (multiple-value-bind (messages-2 _) (receive-message queue-url :base-attributes '("All") 
								   :message-attributes '("Attribute-1" "Attribute-2"))
      (declare (ignore _))
      (assert-equal 1 (length messages-2))
      (let* ((msg (first messages-2))
	     (msg-attributes (message-attributes msg))
	     (attr-1 (find "Attribute-1" msg-attributes :test 'equal :key 'message-attribute-name))
	     (attr-2 (find "Attribute-2" msg-attributes :test 'equal :key 'message-attribute-name)))
	(assert-equal "string attributes" (message-attribute-value attr-1))
	(assert-equal :string (message-attribute-type attr-1))
	(assert-equal "1" (message-attribute-value attr-2))
	(assert-equal :number (message-attribute-type attr-2)))
      (let ((delete-response (delete-message queue-url (message-receipt-handle (first messages-2)))))
	(assert-equal 200 (response-status delete-response))))))

(defun make-batch-messages ()
  (make-instance 'send-message-batch-action
		 :messages (list
			    (make-instance 'batch-message-entry :id "msg-1" :body "msg-1 body"
								:attributes (list (make-message-attribute "Attr1" :string "a1")
										  (make-message-attribute "Attr2" :number 1)))
			    (make-instance 'batch-message-entry
					   :id "msg-2"
					   :body "msg-2 body"
					   :attributes (list
							(make-message-attribute "Attr1"
										:binary "YXNkYXNkKiYqYWxsODI4MzEyPD8nXTg5MDM3c2FrbGRhbWM="))))))

(define-test send-batch-1
  (:tag :sqs :single-thread :batch)
  (let ((queue-url (get-queue-url *queue-name*)))
    (multiple-value-bind (result _) (send-message-batch queue-url (make-batch-messages))
      (declare (ignore _))
      (assert-true (typep result 'batch-request-result))
      (assert-nil (batch-failed result))
      (assert-equal 2 (length (batch-successful result)))
      (let ((msg-1-result (find "msg-1" (batch-successful result) :key #'entry-id :test #'equal)))
	(assert-true (stringp (message-id msg-1-result)))
	(assert-true (stringp (message-body-md5 msg-1-result)))
	(assert-true (stringp (message-attributes-md5 msg-1-result)))))))

(define-test receive-delete-1
  (:tag :sqs :single-thread :batch)
  (let ((queue-url (get-queue-url *queue-name*)))
    (multiple-value-bind (result response) (receive-message queue-url :max 2
								      :base-attributes '("All")
								      :message-attributes '("Attr1" "Attr2"))
      (declare (ignore response))
      (assert-equal 2 (length result))
      (let* ((msg-1 (find "msg-1 body" result :key #'message-body :test 'equal))
	     (msg-1-base-attributes (message-base-attributes msg-1))
	     (msg-1-attributes (message-attributes msg-1))
	     (msg-1-attribute-1 (find "Attr1" msg-1-attributes :key #'message-attribute-name :test #'equal))
	     (msg-2 (find "msg-2 body" result :key #'message-body :test 'equal))
	     (msg-2-base-attributes (message-base-attributes msg-2))
	     (msg-2-attributes (message-attributes msg-2))
	     (msg-2-attribute-1 (find "Attr1" msg-2-attributes :key #'message-attribute-name :test 'equal)))
	(assert-true (> (length msg-1-base-attributes) 3))
	(assert-true (> (length msg-2-base-attributes) 3))
	(assert-equal "a1" (message-attribute-value msg-1-attribute-1))
	(assert-eq :string (message-attribute-type msg-1-attribute-1))
	(assert-equal "YXNkYXNkKiYqYWxsODI4MzEyPD8nXTg5MDM3c2FrbGRhbWM=" (message-attribute-value msg-2-attribute-1))
	(assert-eq :binary (message-attribute-type msg-2-attribute-1))
	(let ((delete-response-1 (delete-message queue-url (message-receipt-handle msg-1)))
	      (delete-response-2 (delete-message queue-url (message-receipt-handle msg-2))))
	  (assert-equal 200 (response-status delete-response-1))
	  (assert-equal 200 (response-status delete-response-2)))))))
