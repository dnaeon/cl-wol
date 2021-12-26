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
(defpackage :cl-wol.test
  (:use :cl :rove))
(in-package :cl-wol.test)

(deftest test-mac-addresses
  (testing "test supported MAC addresses"
    (let ((items (list "00-0B-F8-39-AC-A6"
		       "00-1C-42-0F-B2-4E"
		       "D0-F0-DB-97-46-67"
		       "00-15-5e-b7-10-32"
		       "00-a0-d1-e5-5e-a2"
		       "00-17-7f-45-5f-01"
		       "00:03:EE:73:D4:8F"
		       "00:0A:FD:15:05:9C"
		       "90:b9:7d:30:97:9f")))
      (dolist (item items)
	(ok (cl-wol.core:parse-hex-bytes item) (format nil "parse mac address ~A" item))
	(ok (cl-wol.core:make-magic-packet item) (format nil "make-magic-packet with ~A" item)))))

  (testing "test unsupported MAC addresses"
    (let ((items (list ""
		       "invalid mac address"
		       "00 A0 94 0B 14 66"
		       "08 00 33 5e 2d ea"
		       "00 21 dd 92 f6 e3"
		       "01-02-03-04-XX-YY")))
      (dolist (item items)
	(ng (cl-wol.core:parse-hex-bytes item) (format nil "parse ~A" item))
	(ok (signals (cl-wol.core:make-magic-packet item)) (format nil "make-magic-packet signals with ~A" item))))))

(deftest mac-octets
  (testing "test mac-octets with known addresses"
    (let ((items '((:addr "00-0B-F8-39-AC-A6" :octets #(0 11 248 57 172 166))
		   (:addr "00-a0-d1-e5-5e-a2" :octets #(0 160 209 229 94 162))
		   (:addr "ff-ff-ff-ff-ff-ff" :octets #(255 255 255 255 255 255))
		   (:addr "00:03:EE:73:D4:8F" :octets #(0 3 238 115 212 143))
		   (:addr "00:0A:FD:15:05:9C" :octets #(0 10 253 21 5 156))
		   (:addr "90:b9:7d:30:97:9f" :octets #(144 185 125 48 151 159)))))
      (dolist (item items)
	(let* ((addr (getf item :addr))
	       (octets (getf item :octets))
	       (magic-packet (cl-wol.core:make-magic-packet addr)))
	  (ok (equalp (cl-wol.core:mac-octets magic-packet) octets)
	      (format nil "mac-octets match for ~A" addr)))))))

(deftest encode-payload
  (testing "test encode-payload with known addresses"
    (ok (equalp (cl-wol.core:encode-payload (cl-wol.core:make-magic-packet "00:01:02:03:04:05"))
		#(#xFF #xFF #xFF #xFF #xFF #xFF   ;; Header
		  #x00 #x01 #x02 #x03 #x04 #x05   ;; 1st repetition
		  #x00 #x01 #x02 #x03 #x04 #x05   ;; 2nd repetition
		  #x00 #x01 #x02 #x03 #x04 #x05   ;; ...
		  #x00 #x01 #x02 #x03 #x04 #x05
		  #x00 #x01 #x02 #x03 #x04 #x05
		  #x00 #x01 #x02 #x03 #x04 #x05
		  #x00 #x01 #x02 #x03 #x04 #x05
		  #x00 #x01 #x02 #x03 #x04 #x05
		  #x00 #x01 #x02 #x03 #x04 #x05
		  #x00 #x01 #x02 #x03 #x04 #x05
		  #x00 #x01 #x02 #x03 #x04 #x05
		  #x00 #x01 #x02 #x03 #x04 #x05
		  #x00 #x01 #x02 #x03 #x04 #x05
		  #x00 #x01 #x02 #x03 #x04 #x05
		  #x00 #x01 #x02 #x03 #x04 #x05   ;; ...
		  #x00 #x01 #x02 #x03 #x04 #x05)) ;; 16th repetition
	"encode-payload matches for 00:01:02:03:04:05")
    (ok (equalp (cl-wol.core:encode-payload (cl-wol.core:make-magic-packet "aa-bb-cc-dd-ee-ff"))
		#(#xFF #xFF #xFF #xFF #xFF #xFF   ;; Header
		  #xAA #xBB #xCC #xDD #xEE #xFF   ;; 1st repetition
		  #xAA #xBB #xCC #xDD #xEE #xFF   ;; 2nd repetition
		  #xAA #xBB #xCC #xDD #xEE #xFF   ;; ...
		  #xAA #xBB #xCC #xDD #xEE #xFF
		  #xAA #xBB #xCC #xDD #xEE #xFF
		  #xAA #xBB #xCC #xDD #xEE #xFF
		  #xAA #xBB #xCC #xDD #xEE #xFF
		  #xAA #xBB #xCC #xDD #xEE #xFF
		  #xAA #xBB #xCC #xDD #xEE #xFF
		  #xAA #xBB #xCC #xDD #xEE #xFF
		  #xAA #xBB #xCC #xDD #xEE #xFF
		  #xAA #xBB #xCC #xDD #xEE #xFF
		  #xAA #xBB #xCC #xDD #xEE #xFF
		  #xAA #xBB #xCC #xDD #xEE #xFF
		  #xAA #xBB #xCC #xDD #xEE #xFF   ;; ...
		  #xAA #xBB #xCC #xDD #xEE #xFF)) ;; 16th repetition
	"encode-payload matches for aa-bb-cc-dd-ee-ff")))
