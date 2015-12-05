# Common Lisp  Amazon SQS Client 

## Installation
Since this code is not in quicklisp dists you need to manually download code and then load it with quicklisp or asdf

```
CL-USER> (push "/tmp/amazonsqs/" asdf:*central-registry*)
("/tmp/amazonsqs/" #P"/Users/milan/quicklisp/quicklisp/")
CL-USER> (ql:quickload "amazonsqs")
.......
("amazonsqs")
```

## Usage example

Create aws credentials

```
CL-USER> (defparameter *creds* (make-instance 'awscredentials :access-key "ACCESS_KEY" :secret-key "SECRET_KEY"))
*CREDS*
````
then SQS
```
CL-USER> (defparameter *mysqs* (make-instance 'sqs :aws-credentials *creds*))
*MYSQS*
````
or use PARALLEL-SQS which is caching and reusing connections (one connection per thread, faster then SQS)
```
CL-USER> (defparameter *mysqs* (make-instance 'parallel-sqs :aws-credentials *creds*))
*MYSQS*
````

### Create and list queues

```
CL-USER> (list-queues :sqs *mysqs*)
NIL
#<RESPONSE 200>
CL-USER> (setf *sqs* *mysqs*)
CL-USER> (list-queues)
NIL
#<RESPONSE 200>
(create-queue "testQueue" :attributes '((:name "DelaySeconds" :value 5)))
"http://sqs.us-east-1.amazonaws.com/653067390209/testQueue"
#<RESPONSE 200>
CL-USER> (list-queues)
("http://sqs.us-east-1.amazonaws.com/653067390209/testQueue")
#<RESPONSE 200>
CL-USER> (get-queue-url "testQueue")
"http://sqs.us-east-1.amazonaws.com/653067390209/testQueue"
#<RESPONSE 200>
CL-USER> (get-queue-attributes (get-queue-url "testQueue") '("DelaySeconds"))
(("DelaySeconds" . "5"))
#<RESPONSE 200>
CL-USER> (get-queue-attributes (get-queue-url "testQueue") '("All"))
(("QueueArn" . "arn:aws:sqs:us-east-1:653067390209:testQueue")
 ("ApproximateNumberOfMessages" . "0")
 ("ApproximateNumberOfMessagesNotVisible" . "0")
 ("ApproximateNumberOfMessagesDelayed" . "0")
 ("CreatedTimestamp" . "1449321474") ("LastModifiedTimestamp" . "1449321474")
 ("VisibilityTimeout" . "30") ("MaximumMessageSize" . "262144")
 ("MessageRetentionPeriod" . "345600") ("DelaySeconds" . "5")
 ("ReceiveMessageWaitTimeSeconds" . "0"))
#<RESPONSE 200>
CL-USER> (delete-queue (get-queue-url "testQueue"))
#<RESPONSE 200>
```
### Sending and receiving messages

```
CL-USER> (send-message (get-queue-url "testQueue") "example message body" :attributes '((:name "MessageAttribute-1" :value 10 :type :number)))
((:MESSAGE-ID . "c6e4e2d8-f25a-4eea-8b9d-5b6dcd094530")
 (:ATTRIBUTES-MD5 . "909bdca3008941c20f265b588e20579a")
 (:BODY-MD5 . "337b359654178adbf8782b837261ff66"))
#<RESPONSE 200>
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

## The AMAZONSQS Dictionary:

add-permission queue-url label permissions &key sqs => 

change-message-visibility queue-url receipt-handle visibility-timeout &key sqs => 

change-message-visibility-batch queue-url entries &key sqs =>

create-queue queue-name &key attributes sqs => 

delete-message queue-url receipt-handle &key sqs =>

delete-message-batch queue-url entries &key sqs =>

delete-queue queue-url &key sqs => response

get-queue-attributes queue-url attributes &key sqs =>

get-queue-url queue-name &key sqs =>

list-dead-letter-source-queues queue-url &key sqs =>

list-queues &key prefix sqs =>

purge-queue queue-url &key sqs =>

receive-message queue-url &key max visibility-timeout wait-time attributes message-attributes sqs =>

remove-permission queue-url label &key sqs =>

send-message queue-url message-body &key delay-seconds attributes sqs =>

send-message-batch queue-url entries &key sqs =>

set-queue-attributes queue-url attribute-name attribute-value &key sqs =>