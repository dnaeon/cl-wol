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

(in-package :cl-wol.cli)

(defun wake/handler (cmd)
  "The handler for the `wake' command"
  (unless (clingon:command-arguments cmd)
    (clingon:print-usage-and-exit cmd t))
  (let ((items (mapcar #'cl-wol.core:make-magic-packet (clingon:command-arguments cmd)))
	(address (clingon:getopt cmd :address))
	(port (clingon:getopt cmd :port)))
    (dolist (item items)
      (format t "Waking up ~A ...~&" (cl-wol.core:mac-address item))
      (cl-wol.core:wake item address port))))

(defun wake/options ()
  "Returns the options for the `wake' command"
  (list
   (clingon:make-option :string
			:description "broadcast address to send the magic packet to"
			:short-name #\a
			:long-name "address"
			:initial-value "255.255.255.255"
			:key :address)
   (clingon:make-option :integer
			:description "UDP port to send broadcast packet to"
			:short-name #\p
			:long-name "port"
			:initial-value 7
			:key :port)))

(defun wake/command ()
  "Returns the command for waking up remote systems"
  (clingon:make-command
   :name "wake"
   :description "wakes up remote systems"
   :usage "MAC-ADDRESS ..."
   :handler #'wake/handler
   :options (wake/options)))
