;; Copyright (c) 2021 Marin Atanasov Nikolov <dnaeon@gmail.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer
;;     in this position and unchanged.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)
(defpackage :cl-wol.core
  (:use :cl)
  (:nicknames :wol.core)
  (:import-from
   :cl-ppcre)
  (:import-from
   :usocket)
  (:export
   :mac-address
   :mac-octets
   :encode-payload
   :wake
   :invalid-mac-address
   :*mac-regex*
   :parse-mac-address
   :magic-packet
   :make-magic-packet))
(in-package :cl-wol.core)

(defgeneric mac-address (object)
  (:documentation "Returns the string representation of the MAC
  address associated with the OBJECT"))

(defgeneric mac-octets (object)
  (:documentation "Returns a vector of bytes representing the MAC
  address associated with the OBJECT"))

(defgeneric encode-payload (object)
  (:documentation "Encodes the OBJECT and returns a vector of bytes
  representing the payload for waking up a remote system"))

(defgeneric wake (object address port)
  (:documentation "Wakes up a remote system by encoding the OBJECT and
  sending a broadcast packet to the given ADDRESS and PORT"))

(define-condition invalid-mac-address (simple-error)
  ((mac-address
    :initarg :mac-address
    :initform (error "Must specify MAC address")
    :reader mac-address))
  (:report (lambda (condition stream)
	     (format stream "Invalid MAC address ~A" (mac-address condition))))
  (:documentation "A condition which is signalled upon an invalid MAC address"))

(defparameter *mac-regex*
  "^([\\da-f]{2})[ :-]([\\da-f]{2})[ :-]([\\da-f]{2})[ :-]([\\da-f]{2})[ :-]([\\da-f]{2})[ :-]([\\da-f]{2})$"
  "Regex used to parse MAC addresses")

(defun parse-mac-address (mac-address)
  "Parses a MAC address from the given string identified by MAC-ADDRESS"
  (cl-ppcre:register-groups-bind (aa bb cc dd ee ff)
      (*mac-regex* (string-downcase mac-address))
    (list aa bb cc dd ee ff)))

(defclass magic-packet ()
  ((mac-address
    :initarg :mac-address
    :initform (error "Must specify MAC address")
    :accessor mac-address
    :documentation "Destination MAC address"))
  (:documentation "A class which represents the Magic Packet used to wake remote systems"))

(defmethod print-object ((object magic-packet) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "addr=~A" (mac-address object))))

(defmethod mac-octets ((object magic-packet))
  (map '(vector (unsigned-byte 8)) (lambda (item)
				     (parse-integer item :radix 16))
       (parse-mac-address (mac-address object))))

(defun make-magic-packet (mac-address)
  "Creates a new instance of MAGIC-PACKET with the given MAC-ADDRESS"
  (unless (cl-ppcre:scan *mac-regex* (string-downcase mac-address))
    (error 'invalid-mac-address :mac-address mac-address))
  (make-instance 'magic-packet :mac-address mac-address))

(defmethod encode-payload ((object magic-packet))
  ;; The payload represents 6 bytes of #xFF followed by 16 repetitions
  ;; of the MAC address. Total length of the payload is 102 bytes.
  (let ((payload nil))
    ;; Encode header
    (loop :repeat 6 :do
      (push #xFF payload))
    ;; Encode MAC address
    (loop :repeat 16 :do
      (loop :for byte :across (mac-octets object) :do
	(push byte payload)))
    (make-array 102 :element-type '(unsigned-byte 8) :initial-contents (nreverse payload))))

(defmethod wake ((object magic-packet) address port)
  (let ((payload (encode-payload object))
	(socket (usocket:socket-connect nil nil :protocol :datagram :element-type '(unsigned-byte 8))))
    (setf (usocket:socket-option socket :broadcast) t)
    (usocket:socket-send socket payload nil :host address :port port)
    (usocket:socket-close socket)))
