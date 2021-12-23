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

(defun get-hosts-to-wake (cmd)
  "Returns the list of MAC addresses to wake up based on the provided
options and arguments from the command-line"
  (let ((names (clingon:getopt cmd :names))
	(database (clingon:getopt cmd :database))
	(free-args (clingon:command-arguments cmd))
	(hosts-from-db nil))
    ;; In order to wake up hosts by name we need a database
    (when (and names (not database))
      (error "No database file specified"))
    ;; We have hosts to lookup from the database
    (when (and names database)
      (let ((db-conn (make-db-conn database)))
	(setf hosts-from-db (reduce (lambda (acc name)
				      (let ((host (get-host-from-db db-conn name)))
					(if host
					    (cons (getf host :|addr|) acc)
					    acc)))
				    names
				    :initial-value nil))))
    (concatenate 'list free-args hosts-from-db)))

(defun wake/handler (cmd)
  "The handler for the `wake' command"
  (let ((hosts (get-hosts-to-wake cmd)))
    (unless hosts
      (clingon:print-usage-and-exit cmd t))
    (let ((address (clingon:getopt cmd :address))
	  (port (clingon:getopt cmd :port))
	  (items (mapcar #'cl-wol.core:make-magic-packet hosts)))
      (dolist (item items)
	(format t "Waking up ~A ...~&" (cl-wol.core:mac-address item))
	(cl-wol.core:wake item address port)))))

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
			:key :port)
   (clingon:make-option :list
			:description "host to lookup from the database and wake"
			:short-name #\n
			:long-name "name"
			:key :names)
   (clingon:make-option :filepath
			:description "path to the database file"
			:short-name #\d
			:long-name "database"
			:env-vars '("DATABASE")
			:key :database)))

(defun wake/command ()
  "Returns the command for waking up remote systems"
  (clingon:make-command
   :name "wake"
   :description "wakes up remote systems"
   :usage "[options] MAC-ADDRESS ..."
   :handler #'wake/handler
   :options (wake/options)))
