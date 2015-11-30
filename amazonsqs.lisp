(in-package :amazonsqs)

(alexandria:define-constant +api-version+ "2012-11-05" :test 'equal)
(alexandria:define-constant +default-region+ "sqs.us-east-1.amazonaws.com" :test 'equal)
(alexandria:define-constant +signature-method+ "HmacSHA256" :test 'equal)
(alexandria:define-constant +signature-version+ "2" :test 'equal)


(defclass awscredentials ()
  ((access-key :initarg :access-key :accessor access-key)
   (secret-key :initarg :secret-key :accessor secret-key)))

(defclass sqs ()
  ((aws-credentials :initarg :aws-credentials :accessor sqs-aws-credentials)
   (region :initarg :region :accessor sqs-region :initform +default-region+)
   (protocol :initarg :protocol :accessor sqs-protocol :initform :http)))

(defclass sqs-state ()
  ((streams :accessor sqs-state-streams :initform nil)
   (lock :accessor sqs-state-lock :initform (bordeaux-threads:make-lock "stream lock"))))

(defclass parallel-sqs (sqs)
  ((internal-state :accessor sqs-internal-state :initform (make-instance 'sqs-state))))

(defgeneric close-sqs (sqs))

(defmethod close-sqs ((sqs sqs))
  (values))

(defmethod close-sqs ((parallel-sqs parallel-sqs))
  (let ((sqs-lock (sqs-state-lock (sqs-internal-state parallel-sqs))))
    (bordeaux-threads:with-lock-held (sqs-lock)
      (let ((streams-hash (sqs-state-streams (sqs-internal-state parallel-sqs))))
	(when streams-hash
	  (maphash (lambda (thread stream)
		     (declare (ignore thread))
		     (close stream))
		   streams-hash)
	  (clrhash streams-hash))
	(values)))))

(defgeneric get-sqs-stream (sqs)
  (:documentation "Get stream for this sqs client or nil if stream is not yet cached"))

(defmethod get-sqs-stream ((parallel-sqs parallel-sqs))
  (let ((sqs-lock (sqs-state-lock (sqs-internal-state parallel-sqs))))
    (bt:with-lock-held (sqs-lock)
      (let ((streams-hash (sqs-state-streams (sqs-internal-state parallel-sqs))))
	(and streams-hash
	     (gethash (bordeaux-threads:current-thread) streams-hash))))))

(defgeneric cache-sqs-stream (sqs stream))

(defmethod cache-sqs-stream ((parallel-sqs parallel-sqs) stream)
  (let ((sqs-lock (sqs-state-lock (sqs-internal-state parallel-sqs))))
    (bordeaux-threads:with-lock-held (sqs-lock)
      (let ((streams-hash (sqs-state-streams (sqs-internal-state parallel-sqs))))
	(unless streams-hash
	  (setf (sqs-state-streams (sqs-internal-state parallel-sqs))
		(make-hash-table)))
	(setf (gethash (bordeaux-threads:current-thread)
		       (sqs-state-streams (sqs-internal-state parallel-sqs)))
	      stream)))))
