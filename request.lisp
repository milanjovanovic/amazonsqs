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
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARsek TICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;
;;;; request.lisp

(in-package #:amazonsqs)

(defclass request ()
  ((action :initarg :action :accessor request-action)
   (parameters :initarg :parameters :accessor request-parameters :initform nil)
   (uri :initarg :queue-url :accessor request-uri :initform nil)))

(defun make-request (action parameters &optional uri)
  (let ((request (make-instance 'request :action action :parameters parameters)))
    (when uri
      (setf (request-uri request) (quri:uri uri)))
    request))

(defgeneric process-request (sqs request)
  (:documentation "Generic function that do real work of sending request to amazon and returning something meaningful"))

(defun get-request-uri (sqs request)
  (or (request-uri request)
      (sqs-region sqs)))

(defun sign-request (sqs request)
  (let* ((parameters (request-parameters request))
	 (full-parameters (add-base-parameters-and-encode sqs parameters (request-action request)))
	 (sorted-full-parameters (sort full-parameters #'string< :key #'car))
	 (parameters-as-string (parameters-to-string sorted-full-parameters))
	 (canonical-string (create-canonical-string (get-request-uri sqs request) parameters-as-string))
	 (signature (sign-string canonical-string (secret-key (sqs-aws-credentials sqs)))))
    (setf (request-parameters request)
	  (acons "Signature" (amazon-encode signature) sorted-full-parameters))))

(defun no-encoder (parameter-value encoding)
  (declare (ignore encoding))
  parameter-value)

(defun http-call (hostname parameters cached-stream retry)
  (flet ((drakma-call (stream)
	   (drakma:http-request hostname
				:parameters parameters
				:method :post
				:url-encoder #'no-encoder
				:close nil
				:stream stream)))
    (handler-bind (((and condition
			 (not warning)
			 (not drakma:parameter-error))
		     (lambda (c)
		       (declare (ignorable c))
		       (when cached-stream
			 (ignore-errors (close cached-stream)))
		       (when (and retry cached-stream)
			 (return-from http-call (drakma-call nil))))))
      (drakma-call cached-stream))))

(defmethod process-request ((sqs sqs) (request request))
  (let ((signed-parameters (sign-request sqs request)))
    (multiple-value-bind (response response-status-code _ __ stream)
	(http-call (quri:render-uri (get-request-uri sqs request)) signed-parameters *cached-stream* *do-cache-stream*)
      (declare (ignore _ __))
      (multiple-value-bind (f s) (create-response (cxml:make-source response))
	(when *do-cache-stream*
	  (setf *cached-stream* stream))
	(if s
	    (values f (make-instance 'response :request-id s :status response-status-code))
	    (make-instance 'response :request-id f :status response-status-code))))))

(defmethod process-request ((sqs connection-pooling-sqs) (request request))
  (let ((signed-parameters (sign-request sqs request))
	(cached-stream (get-connection (sqs-connection-pool sqs))))
    (multiple-value-bind (response response-status-code _ __ stream)
	(handler-bind (((not warning) #'(lambda (c)
					  (declare (ignore c))
					  (ignore-errors (close cached-stream))
					  (add-connection (sqs-connection-pool sqs) nil))))
	  (http-call (quri:render-uri (get-request-uri sqs request)) signed-parameters cached-stream t))
      (declare (ignore _ __))
      (add-connection (sqs-connection-pool sqs) stream)
      (multiple-value-bind (f s) (create-response (cxml:make-source response))
	(if s
	    (values f (make-instance 'response :request-id s :status response-status-code))
	    (make-instance 'response :request-id f :status response-status-code))))))

(defun create-canonical-string (uri parameters)
  (format nil "~A~%~A~%~A~%~A"
	  "POST" (quri:uri-host uri) 
	  (or (quri:uri-path uri) "/")
	  parameters))

(defun add-base-parameters-and-encode (sqs params action)
  (append `(("Action" . ,(amazon-encode action))
	    ("Version" . ,(amazon-encode +api-version+))
	    ("AWSAccessKeyId" . ,(amazon-encode (access-key (sqs-aws-credentials sqs))))
	    ("SignatureVersion" . ,(amazon-encode +signature-version+))
	    ("SignatureMethod" . ,(amazon-encode +signature-method+))
	    ("Timestamp" . ,(amazon-encode (iso8601-time))))
	  (mapcar (lambda (cons)
		    (cons (amazon-encode (car cons))
			  (amazon-encode (cdr cons))))
		  params)))

(defun parameters-to-string (parameters)
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (loop for (key . value) in parameters
	    for index  from 0
	    do
	       (format s "~:[~;&~]~A=~A"(> index 0) key value)))))
