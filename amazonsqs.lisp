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
  ((%internal-state :accessor %sqs-internal-state :initform (make-instance 'sqs-state))))

(defgeneric close-sqs (sqs))

(defmethod close-sqs ((sqs sqs))
  (values))

(defmethod close-sqs ((parallel-sqs parallel-sqs))
  (let ((sqs-lock (sqs-state-lock (%sqs-internal-state parallel-sqs))))
    (bordeaux-threads:with-lock-held (sqs-lock)
      (let ((streams-hash (sqs-state-streams (%sqs-internal-state parallel-sqs))))
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
  (let ((sqs-lock (sqs-state-lock (%sqs-internal-state parallel-sqs))))
    (bt:with-lock-held (sqs-lock)
      (let ((streams-hash (sqs-state-streams (%sqs-internal-state parallel-sqs))))
	(and streams-hash
	     (gethash (bordeaux-threads:current-thread) streams-hash))))))

(defgeneric cache-sqs-stream (sqs stream))

(defmethod cache-sqs-stream ((parallel-sqs parallel-sqs) stream)
  (let ((sqs-lock (sqs-state-lock (%sqs-internal-state parallel-sqs))))
    (bordeaux-threads:with-lock-held (sqs-lock)
      (let ((streams-hash (sqs-state-streams (%sqs-internal-state parallel-sqs))))
	(unless streams-hash
	  (setf (sqs-state-streams (%sqs-internal-state parallel-sqs))
		(make-hash-table)))
	(setf (gethash (bordeaux-threads:current-thread)
		       (sqs-state-streams (%sqs-internal-state parallel-sqs)))
	      stream)))))
