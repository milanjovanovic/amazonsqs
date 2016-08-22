# Common Lisp  Amazon SQS Client 

## Installation
Since this code is not in quicklisp dists you need to manually download code and then load it with quicklisp or asdf

```
CL-USER> (push "/tmp/amazonsqs/" asdf:*central-registry*)
("/tmp/amazonsqs/" #P"/Users/milan/quicklisp/quicklisp/")
CL-USER> (ql:quickload "amazonsqs")
.......
("amazonsqs")
CL-USER> (use-package :amazonsqs)
T
```

## Usage example

Create aws credentials

```
CL-USER> (defparameter *creds* (make-instance 'awscredentials :access-key "ACCESS_KEY" :secret-key "SECRET_KEY"))
*CREDS*
```
then create sqs client using **SQS** class (default behaviour of this client is opening and closing connections on every request)
```
CL-USER> (defparameter *mysqs* (make-instance 'sqs :aws-credentials *creds*))
*MYSQS*
```
or using **CONNECTION-POOLING-SQS** class
```
CL-USER> (defparameter *mysqs* (make-instance 'connection-pooling-sqs :aws-credentials *creds* :pool-size 5))
*MYSQS*
```
### Multi-threaded usage
For connection-per-thread **SQS** client class can be used with \***DO-CACHE-STREAM**\* set to **T**. Stream is cached into \***CACHED-STREAM**\* var.

There is macro **WITH-CACHED-STREAM** that ensure per-thread binding that can be used with SQS client class.
When not using  **WITH-CACHED-STREAM** rebinding in every thread is necessary (closing of client also)

```
(sb-thread:make-thread (lambda ()
                      ;; assuming that \***DO-CACHE-STREAM**\* is bind to T
				   (let ((*cached-stream* nil))
				     (dotimes (i 100)
				       (send-message queue-url "msg body" :sqs *simple-sqs*))
				       ;; need to close client in every thread before exiting 
				     (close-sqs *simple-sqs*))))
				     
```				      


**WITH-CACHED-STREAM** example:
```
(progn
	   (dotimes (i 10)
	     (sb-thread:make-thread  (lambda ()
				       (with-cached-stream
					 (dotimes (i 100)
					   (send-message *queue-url* "message body" :sqs *simple-sqs*))))))
	   (dotimes (i 10)
	     (with-cached-stream
	       (dotimes (i 100)
		 (send-message *queue-url* "message body" :sqs *simple-sqs*)))))
		 
```


### Connection Pooling Multi-threaded client

Much simpler (no special macros,no rebinding in threads and no connection-per-thread) is to use thread-safe **CONNECTION-POOLING-SQS** client which maintains pool of connections.


### Basic Queue Operations

**Every API call accepts ``&key sqs``, if not supplied ```*sqs*``` is used**

List queues:
```
CL-USER> (list-queues :sqs *mysqs*)
NIL
#<RESPONSE 200>
CL-USER> (setf *sqs* *mysqs*)
CL-USER> (list-queues)
NIL
```
Create queue:
```
CL-USER> (create-queue "testQueue" :attributes '((:name "DelaySeconds" :value 5)))
"http://sqs.us-east-1.amazonaws.com/0123456789/testQueue"
#<RESPONSE 200>
CL-USER> (list-queues)
("http://sqs.us-east-1.amazonaws.com/0123456789/testQueue")
#<RESPONSE 200>
```

Getting queue url and attributes:
```
CL-USER> (get-queue-url "testQueue")
"http://sqs.us-east-1.amazonaws.com/0123456789/testQueue"
#<RESPONSE 200>
CL-USER> (get-queue-attributes (get-queue-url "testQueue") '("DelaySeconds"))
(("DelaySeconds" . "5"))
#<RESPONSE 200>
CL-USER> (get-queue-attributes (get-queue-url "testQueue") '("All"))
(("QueueArn" . "arn:aws:sqs:us-east-1:0123456789:testQueue")
 ("ApproximateNumberOfMessages" . "0")
 ("ApproximateNumberOfMessagesNotVisible" . "0")
 ("ApproximateNumberOfMessagesDelayed" . "0")
 ("CreatedTimestamp" . "1449321474") ("LastModifiedTimestamp" . "1449321474")
 ("VisibilityTimeout" . "30") ("MaximumMessageSize" . "262144")
 ("MessageRetentionPeriod" . "345600") ("DelaySeconds" . "5")
 ("ReceiveMessageWaitTimeSeconds" . "0"))
#<RESPONSE 200>
```
Delete queue:
```
CL-USER> (delete-queue (get-queue-url "testQueue"))
#<RESPONSE 200>
```

### Sending and receiving messages

Sending one message:
```
CL-USER> (send-message (get-queue-url "testQueue") "example message body" :attributes '((:name "MessageAttribute-1" :value 10 :type :number)))
((:MESSAGE-ID . "c6e4e2d8-f25a-4eea-8b9d-5b6dcd094530")
 (:ATTRIBUTES-MD5 . "909bdca3008941c20f265b588e20579a")
 (:BODY-MD5 . "337b359654178adbf8782b837261ff66"))
#<RESPONSE 200>
```
Receive and delete message:
```
CL-USER> (defparameter *queue-url* (get-queue-url "testQueue"))
*QUEUE-URL*
CL-USER> (receive-message *queue-url* :max 10 :attributes '("All") :message-attributes '("MessageAttribute-1"))
(#<MESSAGE examp... {10092419D3}>)
#<RESPONSE 200>
CL-USER> (defparameter *received-msgs* *)
*RECEIVED-MSGS*
CL-USER> (first *received-msgs*)
#<MESSAGE examp... {1004409383}>
CL-USER> (message-body (first *received-msgs*))
"example message body"
CL-USER> (message-attributes (first *received-msgs*))
(#<MESSAGE-ATTRIBUTE MessageAttribute-1>)
CL-USER> (attributes (first *received-msgs*))
(("SentTimestamp" . "1449321567628") ("ApproximateReceiveCount" . "2")
 ("ApproximateFirstReceiveTimestamp" . "1449321656050")
 ("SenderId" . "AIDAJC4FX3MM62J3KPCT4"))
 CL-USER> (delete-message *queue-url* (message-receipt-handle (first *received-msgs*)))
#<RESPONSE 200>
```

Sending more than one message in one request:

```
CL-USER> (send-message-batch *queue-url* '((:id "id1" :body "1. msg body")
					   (:id "id2" :body "2. msg body" :delay-seconds 10)
					   (:id "id3" :body "3. msg body" :attributes ((:name "attr1" :type :number :value 10)))))


#<BATCH-REQUEST-RESULT :successful 3, failed: 0 {1003CF87C3}>
#<RESPONSE 200>
CL-USER> 
CL-USER> (batch-successful *)
(#<SEND-MESSAGE-BATCH-RESULT id1> #<SEND-MESSAGE-BATCH-RESULT id2>
 #<SEND-MESSAGE-BATCH-RESULT id3>)
```
or the same thing with CLOS objects:
```
CL-USER> (defparameter *send-message-action* (make-instance 'send-message-batch-action))
*SEND-MESSAGE-ACTION*
CL-USER> (add-message-entry *send-message-action* (make-instance 'batch-message-entry :id "id100"  :body "another msg"))

(#<BATCH-MESSAGE-ENTRY {1006C78923}>)
CL-USER> (add-message-entry *send-message-action* (make-instance 'batch-message-entry :id "id200"  :body "another msg"
								 :attributes (list
									      (make-instance 'message-attribute 
											     :type :string
											     :value "foo"
											     :name "AttrBatchName"))))
(#<BATCH-MESSAGE-ENTRY {1007007903}> #<BATCH-MESSAGE-ENTRY {1006C78923}>)
CL-USER> (send-message-batch *queue-url* *send-message-action*)
#<BATCH-REQUEST-RESULT :successful 2, failed: 0 {10043C1383}>
#<RESPONSE 200>
CL-USER> 

```
deleting more than one message:
```
CL-USER> (defparameter *received-messages* (receive-message *queue-url* :max 5))
*RECEIVED-MESSAGES*
CL-USER> (defparameter *delete-message-batch-action* (make-instance 'delete-message-batch-action))
*DELETE-MESSAGE-BATCH-ACTION*
CL-USER> (add-message-entry *delete-message-batch-action* 
			    (make-instance 'batch-message-delete-entry 
					   :id "message-1"
					   :receipt-handle (message-receipt-handle (first *received-messages*))))
(#<BATCH-MESSAGE-DELETE-ENTRY {1008A49A13}>)
CL-USER> (add-message-entry *delete-message-batch-action* 
			    (make-instance 'batch-message-delete-entry 
					   :id "message-2"
					   :receipt-handle (message-receipt-handle (second *received-messages*))))
(#<BATCH-MESSAGE-DELETE-ENTRY {1008A70AB3}>
 #<BATCH-MESSAGE-DELETE-ENTRY {1008A49A13}>)
 CL-USER> (delete-message-batch *queue-url* *delete-message-batch-action*)
#<BATCH-REQUEST-RESULT :successful 2, failed: 0 {100A1C1383}>
#<RESPONSE 200>
CL-USER> (batch-successful *)
(#<DELETE-MESSAGE-BATCH-RESULT message-1>
 #<DELETE-MESSAGE-BATCH-RESULT message-2>)
CL-USER> 
```
the same without CLOS:
```
CL-USER> (delete-message-batch *queue-url* `((:id "ID1" :receipt-handle ,(message-receipt-handle (first *received-messages*)))
					     (:id "ID2" :receipt-handle ,(message-receipt-handle (second *received-messages*)))))
#<BATCH-REQUEST-RESULT :successful 2, failed: 0 {10049BA243}>
#<RESPONSE 200>
CL-USER> (batch-successful *)
(#<DELETE-MESSAGE-BATCH-RESULT ID1> #<DELETE-MESSAGE-BATCH-RESULT ID2>)
CL-USER> 
```


## The AMAZONSQS Dictionary:

**NOTE** All methods/functions described here that operates on Amazon SQS are directly mapped to Actions from [Amazon SQS documentation](http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_Operations.html)

### Classes

*class*
**AWSCREDENTIALS**

***

*class*
**SQS**

***

*class*
**CONNECTION-POOLING-SQS**

***

*class*
**MESSAGE**


*accessor* **message-id**

*accessor* **message-body**

*accessor* **message-receipt-handle**

*accessor* **mesage-body-md5**

*accessor* **message-attributes**

*accessor* **attributes**

*accessor* **message-attributes-md5**
***

*class*
**BATCH-REQUEST-RESULT**

Object of this class is returned when one of **batch** requests are called

*accessor* 
**batch-successful**

*accessor*
**batch-failed**
***

*class*
**BATCH-ERROR-RESULT**
***

*class*
**DELETE-MESSAGE-BATCH-RESULT**
***

*class*
**SEND-MESSAGE-BATCH-RESULT**
***

*class*
**CHANGE-MESSAGE-VISIBILITY-BATCH-RESULT**
***

*class*
**SEND-MESSAGE-BATCH-ACTION**
***

*class*
**DELETE-MESSAGE-BATCH-ACTION**
***

*class*
**MESSAGE-ATTRIBUTE**
***

*class*
**BATCH-MESSAGE-ENTRY**
***

*class*
**BATCH-MESSAGE-DELETE-ENTRY**



### Functions/Methods
**add-permission** queue-url label permissions &key sqs => response

**change-message-visibility** queue-url receipt-handle visibility-timeout &key sqs => response

**change-message-visibility-batch** queue-url entries &key sqs =>

**create-queue** queue-name &key attributes sqs => 

**delete-message** queue-url receipt-handle &key sqs =>

**delete-message-batch** queue-url entries &key sqs =>

**delete-queue** queue-url &key sqs => response

**get-queue-attributes** queue-url attributes &key sqs =>

**get-queue-url** queue-name &key sqs =>

**list-dead-letter-source-queues** queue-url &key sqs =>

**list-queues** &key prefix sqs =>

**purge-queue** queue-url &key sqs =>

**receive-message** queue-url &key max visibility-timeout wait-time attributes message-attributes sqs =>

**remove-permission** queue-url label &key sqs =>

**send-message** queue-url message-body &key delay-seconds attributes sqs =>

**send-message-batch** queue-url entries &key sqs =>

**set-queue-attributes** queue-url attribute-name attribute-value &key sqs =>