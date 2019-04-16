(in-package :amazonsqs)

(defstruct (queue
	    (:constructor make-queue (size &aux (data (make-array size :initial-element nil))
						(head 0)
						(count 0))))
  size data head count)

(defun queue-full-p (queue)
  (= (queue-count queue)
     (queue-size queue)))

(defun queue-empty-p (queue)
  (= (queue-count queue) 0))

(defun queue-add (queue data)
  (if (not (queue-full-p queue))
      (let* ((data-vec (queue-data queue))
	     (tail (mod (+ (queue-count queue)
			   (queue-head queue))
			(queue-size queue))))
	(setf (svref data-vec tail) data)
	(incf (queue-count queue))
	(values data t))
      (values data nil)))

(defun queue-get (queue)
  (if (not (queue-empty-p queue))
      (let* ((data-vec (queue-data queue))
	     (head (queue-head queue))
	     (data (svref data-vec head)))
	(setf (svref data-vec head) nil
	      (queue-head queue) (mod (1+ head) (queue-size queue)))
	(decf (queue-count queue))
	(values data t))
      (values nil nil)))

(defclass connection-pool ()
  ((size :initarg :size :accessor pool-size)
   (queue :initarg :queue :accessor pool-queue)
   (lock :accessor pool-lock :initform (bt:make-lock "connection pool lock"))
   (condition-var :accessor pool-condition-var
		  :initform (bt:make-condition-variable :name "connection pool condition var"))
   (free :initarg :free :accessor pool-free)))

(defun make-connection-pool (size)
  (make-instance 'connection-pool :size size :queue (make-queue size) :free size))

(defgeneric get-connection (pool))

(defmethod get-connection ((pool connection-pool))
  (bt:with-lock-held ((pool-lock pool))
    (loop
      (when (> (pool-free pool) 0)
	(decf (pool-free pool))
	(return (queue-get (pool-queue pool))))
      (bt:condition-wait (pool-condition-var pool) (pool-lock pool)))))

(defgeneric add-connection (pool data))

(defmethod add-connection ((pool connection-pool) data)
  (bt:with-lock-held ((pool-lock pool))
    (if (not (queue-full-p (pool-queue pool)))
	(progn
	  (queue-add (pool-queue pool) data)
	  (incf (pool-free pool))
	  (bt:condition-notify (pool-condition-var pool)))
	;; FIXME, don't signal error
	(error "Queue is full"))))

(defgeneric close-pool (pool))

(defmethod close-pool ((pool connection-pool))
  (bt:with-lock-held ((pool-lock pool))
    (let ((queue (pool-queue pool)))
      (do ((stream (queue-get queue) (queue-get queue)))
	  ((null stream) pool)
	(when stream
	  (ignore-errors (close stream)))))))
