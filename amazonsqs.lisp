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
;;;; amazonsqs.lisp

(in-package #:amazonsqs)

(alexandria:define-constant +api-version+ "2012-11-05" :test 'equal)
(alexandria:define-constant +default-region+ "http://sqs.us-east-1.amazonaws.com" :test 'equal)
(alexandria:define-constant +signature-method+ "HmacSHA256" :test 'equal)
(alexandria:define-constant +signature-version+ "2" :test 'equal)

(defparameter *cached-stream* nil)
(defparameter *do-cache-stream* nil)

(defclass awscredentials ()
  ((access-key :initarg :access-key :accessor access-key)
   (secret-key :initarg :secret-key :accessor secret-key)))

(defun valid-region-p (region)
  (or
   (search "http://" region :start2 0 :end2 8)
   (search "https://" region :start2 0 :end2 8)))

(defclass sqs () 
  ((aws-credentials :initarg :aws-credentials
		    :accessor sqs-aws-credentials
		    :initform (error "Need aws-credentials !!!"))
   (region :initarg :region
	   :accessor sqs-region
	   :initform +default-region+)))

(defclass connection-pooling-sqs (sqs)
  ((pool-size :initarg :pool-size
	      :accessor sqs-pool-size
	      :initform (error "Need pool size value !!!"))
   (connection-pool :accessor sqs-connection-pool)))

(defmethod initialize-instance :after ((sqs sqs)
				       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((region (sqs-region sqs)))
    (if (valid-region-p region)
	(setf (sqs-region sqs) (quri:uri region))
	(error "Region must start with http or https !!!"))))

(defmethod initialize-instance :after ((sqs connection-pooling-sqs)
				       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (sqs-connection-pool sqs)
	(make-connection-pool (sqs-pool-size sqs))))

(defgeneric close-sqs (sqs))

(defmethod close-sqs ((sqs sqs))
  (when *cached-stream*
    (ignore-errors (close *cached-stream*))
    (values)))

(defmethod close-sqs ((sqs connection-pooling-sqs))
  (close-pool (sqs-connection-pool sqs)))
