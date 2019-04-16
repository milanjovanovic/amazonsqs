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
;;;; parameters.lisp

(in-package #:amazonsqs)

(defgeneric make-parameters-combo (vaue index base-names suffixes))

(defmethod make-parameters-combo ((value list) index base-names suffixes)
  (mapcan (lambda (v base-name suffix)
	    (make-parameters-combo v index base-name suffix))
	  value base-names suffixes))

(defmethod make-parameters-combo ((value t) index base-name suffix)
  (when value
    (list (cons (concatenate 'string  base-name (write-to-string index) suffix)
		value))))

(defun list-to-indexed-parameters (values base-name &optional suffix)
  (let ((suffix (or suffix "")))
    (mapcan (lambda (value index)
	      (make-parameters-combo value index base-name suffix))
	    values (alexandria:iota (length values) :start 1))))

(defun transform-parameters-plists (mark params)
  (mapcar (lambda (param)
	    (parse-parameter-plist mark param))
	  params))

(defun parse-parameter-plist (mark param)
  (loop for (mark needs) on mark by #'cddr
	for value = (getf param mark)
	for result = (if value
			 value
			 (if needs
			     (error 'bad-parameters :msg (format nil "Missing parameter ~A in ~A" mark param))
			     nil))
	collecting result))

(defun create-complex-n-member-parameters (complex-values base-names &optional suffixes)
  (let* ((elem-length (length (first complex-values)))
	 (suffixes (or suffixes (make-list elem-length :initial-element "")))
	 (base-names (if (listp base-names) base-names (make-list elem-length :initial-element base-names))))
    (mapcan (lambda (one-value index)
	      (make-parameters-combo one-value index base-names suffixes))
	    complex-values
	    (alexandria:iota (length complex-values) :start 1))))

(defmethod create-message-attribute-parameters (prefix index plist))

(defmethod create-message-attribute-parameters (prefix index (attr message-attribute))
  (flet ((parameter-name (field)
	   (format nil "~A~A.~A.~A" prefix
		   "MessageAttribute" index field)))
    (let ((atype (attribute-type-as-string (message-attribute-type attr)))
	  (avalue-name (attribute-type-as-value-type-string (message-attribute-type attr))))
      (list (cons (parameter-name "Name") (message-attribute-name attr))
	    (cons (parameter-name avalue-name) (message-attribute-value attr) )
	    (cons (parameter-name "Value.DataType") atype)))))

(defmethod create-message-attribute-parameters (prefix index (plist list))
  (flet ((parameter-name (field)
	   (format nil "~A~A.~A.~A" prefix
		   "MessageAttribute" index field)))
    (let (aname avalue atype avalue-name)
      (loop for (key value) on plist by #'cddr
	    do
	       (case key
		 (:name
		  (setf aname value))
		 (:value
		  (setf avalue value))
		 (:type
		  (setf atype (attribute-type-as-string value))
		  (setf avalue-name (attribute-type-as-value-type-string value)))
		 (otherwise)))
      (unless (and aname avalue atype avalue-name)
	(error 'bad-parameters :msg (format nil "Bad arguments ~A" plist)))
      (list (cons (parameter-name "Name") aname)
	    (cons (parameter-name avalue-name) avalue)
	    (cons (parameter-name "Value.DataType") atype)))))

(defun create-all-message-attributes-parameters (plist prefix)
  (mapcan
   (lambda (attr-plist attribute-index)
     (create-message-attribute-parameters prefix
					  attribute-index
					  attr-plist))
   plist
   (alexandria:iota (length plist) :start 1)))

(defun create-message-batch-entry-parameters (plist index)
  (let (id body delay-seconds attributes-parameters)
    (loop for (key value) on plist by #'cddr
	  do
	     (ecase key
	       (:id (setf id value))
	       (:body (setf body value))
	       (:delay-seconds (setf delay-seconds value))
	       (:attributes (setf attributes-parameters (create-all-message-attributes-parameters
							 value
							 (format nil "SendMessageBatchRequestEntry.~A." index))))
	       (otherwise)))
    (unless (and id body)
      (error 'bad-parameters :msg (format nil "Bad arguments ~A" plist)))
    (nconc (alist-if-not-nil (format nil "SendMessageBatchRequestEntry.~A.Id" index) id
			     (format nil "SendMessageBatchRequestEntry.~A.MessageBody" index) body
			     (format nil "SendMessageBatchRequestEntry.~A.DelaySeconds" index) delay-seconds)
	   attributes-parameters)))

(defun create-send-message-batch-parameters (plists)
  (mapcan (lambda (plist index)
	    (create-message-batch-entry-parameters plist index))
	  plists
	  (alexandria:iota (length plists) :start 1)))
