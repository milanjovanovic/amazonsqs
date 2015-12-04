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
;;;; parser.lisp

(in-package #:amazonsqs)

(defun parse-sqs-response (source schema &optional initial-value start-element-type)
  (let ((initial-value (or initial-value (schema-initial-call schema)))
	(type (or start-element-type
		  (klacks:peek-next source))))
    (cond ((eq type :START-ELEMENT)
	   (let* ((name (klacks:current-lname source))
		  (callback (schema-name-start-fun schema name)))
	     (if callback
		 (parse-sqs-response source schema (funcall callback (get-next-value source) initial-value) (klacks:peek source))
		 (parse-sqs-response source schema initial-value))))
	  ((eq type :END-ELEMENT)
	   (let* ((name (klacks:current-lname source))
		  (callback (schema-name-end-fun schema name)))
	     (if callback
		 (parse-sqs-response source schema (funcall callback initial-value))
		 (parse-sqs-response source schema initial-value))))
	  ((eq type :END-DOCUMENT)
	   (schema-return-call schema initial-value))
	  (t
	   (parse-sqs-response source schema initial-value)))))

(defun create-response (source)
  (let ((type (klacks:peek source)))
    (cond ((eq type :START-ELEMENT)
	   (let* ((lname (klacks:current-lname source))
		  (schema (get-schema lname)))
	     (unless schema
	       (error 'parsing-error :msg (format nil "Unknown schema for response: ~A" lname)))
	     (parse-sqs-response source schema nil type)))
	  ((eq type :END-DOCUMENT) 
	   (error 'parsing-error :msg "Unknown response schema !!!"))
	  (t
	   (progn
	     (klacks:peek-next source)
	     (create-response source))))))

(defun get-next-value (source)
  (let ((type (klacks:peek-next source)))
    (if (eq type :CHARACTERS)
	(remove-blanks (klacks:current-characters source))
	nil)))
