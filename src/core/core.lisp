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
   :secureon-password
   :wake
   :invalid-mac-address
   :invalid-password
   :invalid-payload
   :payload
   :*mac-regex*
   :parse-hex-bytes
   :magic-packet
   :make-magic-packet
   :make-octet-vector))
(in-package :cl-wol.core)

(deftype simple-octet-vector (&optional length)
  (let ((length (or length '*)))
    `(simple-array (unsigned-byte 8) (,length))))

(defun make-octet-vector (contents)
  "Make an octet vector from the given contents"
  (make-array (length contents) :element-type '(unsigned-byte 8) :initial-contents contents))

(defgeneric mac-address (object)
  (:documentation "Returns the string representation of the MAC
  address associated with the OBJECT"))

(defgeneric mac-octets (object)
  (:documentation "Returns a vector of bytes representing the MAC
  address associated with the OBJECT"))

(defgeneric secureon-password (object)
  (:documentation "Returns the SecureOn password associated with the OBJECT as a vector of octets"))

(defgeneric encode-payload (object)
  (:documentation "Encodes the OBJECT and returns a vector of bytes
  representing the payload for waking up a remote system"))

(defgeneric wake (object address port)
  (:documentation "Wakes up a remote system by encoding the OBJECT and
  sending a broadcast packet to the given ADDRESS and PORT"))

(defgeneric make-magic-packet (address &optional password)
  (:documentation "Creates a new magic packet destined to the given
  ADDRESS. If PASSWORD is specified it represents a SecureOn
  password"))

(define-condition invalid-mac-address (simple-error)
  ((address
    :initarg :address
    :initform (error "Must specify MAC address")
    :reader mac-address))
  (:report (lambda (condition stream)
             (format stream "Invalid MAC address ~A" (mac-address condition))))
  (:documentation "A condition which is signalled upon an invalid MAC address"))

(define-condition invalid-password (simple-error)
  ((password
    :initarg :password
    :initform (error "Must specify password")
    :reader secureon-password))
  (:report (lambda (condition stream)
             (format stream "Invalid SecureOn password ~A" (secureon-password condition))))
  (:documentation "A condition which is signalled upon invalid SecureOn password"))

(define-condition invalid-payload (simple-error)
  ((payload
    :initarg :payload
    :initform (error "Must specify payload")
    :reader payload))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid payload generated")))
  (:documentation "A condition which is signalled when invalid payload is generated"))

(defparameter *mac-regex*
  "^([\\da-f]{2})[:-]([\\da-f]{2})[:-]([\\da-f]{2})[:-]([\\da-f]{2})[:-]([\\da-f]{2})[:-]([\\da-f]{2})$"
  "Regex used to parse MAC addresses")

(defun parse-hex-bytes (str)
  "Parses a string representation of a MAC address or SecureOn password into a list of bytes"
  (cl-ppcre:register-groups-bind (aa bb cc dd ee ff)
      (*mac-regex* (string-downcase str))
    (map '(vector (unsigned-byte 8))
         (lambda (item)
           (parse-integer item :radix 16))
         (list aa bb cc dd ee ff))))

(defclass magic-packet ()
  ((address
    :initarg :address
    :initform (error "Must specify MAC address")
    :reader mac-octets
    :documentation "Destination MAC address")
   (password
    :initarg :password
    :initform nil
    :reader secureon-password
    :documentation "Optional SecureOn password"))
  (:documentation "A class which represents the Magic Packet used to wake remote systems"))

(defmethod print-object ((object magic-packet) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "addr=~A" (mac-address object))))

(defmethod mac-address ((object magic-packet))
  (with-output-to-string (s)
    (loop :for (byte . rest) :on (map 'list #'identity (mac-octets object)) :do
      (if rest
          (format s "~2,'0x:" byte)
          (format s "~2,'0x" byte)))))

(defmethod make-magic-packet ((address string) &optional password)
  (check-type password (or null string (simple-octet-vector 6)))
  (let ((addr-octets (parse-hex-bytes address))
        (pass-octets (etypecase password
                       (string (parse-hex-bytes password))
                       ((simple-octet-vector 6) password)
                       (null nil))))
    ;; Validate MAC address
    (unless addr-octets
      (error 'invalid-mac-address :address address))
    ;; Validate SecureOn password
    (when (and password (not pass-octets))
      (error 'invalid-password :password password))
    (make-instance 'magic-packet :address addr-octets :password pass-octets)))

(defmethod make-magic-packet (address &optional password)
  (check-type address (simple-octet-vector 6))
  (check-type password (or null (simple-octet-vector 6)))
  (make-instance 'magic-packet :address address :password password))

(defmethod encode-payload ((object magic-packet))
  ;; The payload represents 6 bytes of #xFF followed by 16 repetitions
  ;; of the MAC address. Total length of the payload is 102 bytes.
  ;; If we have SecureOn password then we have another 6 bytes to
  ;; represent the password itself, resulting in 108 bytes in total.
  (let ((payload nil))
    ;; Encode header
    (loop :repeat 6 :do
      (push #xFF payload))
    ;; Encode MAC address
    (loop :repeat 16 :do
      (loop :for byte :across (mac-octets object) :do
        (push byte payload)))
    ;; Encode SecureOn password
    (when (secureon-password object)
      (loop :for byte :across (secureon-password object) :do
        (push byte payload)))
    ;; Validate and return payload
    (let ((payload-length (length payload)))
      (unless (or (= payload-length 102) (= payload-length 108))
        (error 'invalid-payload :payload payload))
      (make-array payload-length :element-type '(unsigned-byte 8) :initial-contents (nreverse payload)))))

(defmethod wake ((object magic-packet) address port)
  (let* ((payload (encode-payload object))
         (size (length payload))
         (socket (usocket:socket-connect nil nil :protocol :datagram :element-type '(unsigned-byte 8))))
    (setf (usocket:socket-option socket :broadcast) t)
    (usocket:socket-send socket payload size :host address :port port)
    (usocket:socket-close socket)))
