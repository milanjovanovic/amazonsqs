(in-package :amazonsqs)

(defstruct (queue
	    (:constructor make-queue (size &aux (data (make-array size :initial-element nil))
					   (head-index 0)
					   (tail-index 0)
					   (count 0))))
  size data head-index tail-index count)

(defun queue-full-p (queue)
  (= (queue-count queue)
     (queue-size queue)))

(defun queue-empty-p (queue)
  (= (queue-count queue) 0))

(defun queue-add (queue data)
  (when (not (queue-full-p queue))
    (let* ((data-vec (queue-data queue))
	   (tail-index (queue-tail-index queue)))
      (setf (svref data-vec tail-index) data
	    (queue-tail-index queue) (mod (1+ tail-index) (queue-size queue)))
      (incf (queue-count queue))
      data)))

(defun queue-get (queue)
  (when (not (queue-empty-p queue))
    (let* ((data-vec (queue-data queue))
	   (head-index (queue-head-index queue))
	   (data (svref data-vec head-index)))
      (setf (svref data-vec head-index) nil
	    (queue-head-index queue) (mod (1+ head-index) (queue-size queue)))
      (decf (queue-count queue))
      data)))

(defclass connection-pool ()
  ((size :initarg :size :accessor pool-size)
   (queue :initarg :queue :accessor pool-queue)
   (free :initarg :free :accessor pool-free)
   (lock :accessor pool-lock :initform (bt:make-lock "connection pool lock"))
   (condition-var :accessor pool-condition-var :initform (bt:make-condition-variable :name "connection pool condition var"))))

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
    (when (not (queue-full-p (pool-queue pool)))
      (queue-add (pool-queue pool) data)
      (incf (pool-free pool))
      (bt:condition-notify (pool-condition-var pool)))))

(defgeneric increase-free (pool))

(defmethod increase-free ((pool connection-pool))
  (bt:with-lock-held ((pool-lock pool))
    (incf (pool-free pool))))

(defgeneric close-pool (pool))

(defmethod close-pool ((pool connection-pool))
  (bt:with-lock-held ((pool-lock pool))
    (do ((free (pool-free pool) (decf (pool-free pool))))
	((= free 0))
      (ignore-errors (close (queue-get (pool-queue pool)))))))
