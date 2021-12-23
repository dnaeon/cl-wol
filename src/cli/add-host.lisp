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

(defun add-host/handler (cmd)
  "Handler for the `add-host' command"
  (let* ((database (clingon:getopt cmd :database))
	 (name (clingon:getopt cmd :name))
	 (address (clingon:getopt cmd :address))
	 (db-conn (make-db-conn database)))
    (unless (cl-wol.core:parse-mac-address address)
      (error 'cl-wol.core:invalid-mac-address :mac-address address))
    (when (get-host-from-db db-conn name)
      (error "Host with name ~A already exists" name))
    (db-execute (make-db-conn database) "INSERT INTO hosts (name, addr) VALUES (?, ?)" name address)))

(defun add-host/options ()
  "Returns the options of the `add-host' command"
  (list
   (clingon:make-option :filepath
			:description "path to the database file"
			:short-name #\d
			:long-name "database"
			:env-vars '("DATABASE")
			:required t
			:key :database)
   (clingon:make-option :string
			:description "name of the host to add"
			:short-name #\n
			:long-name "name"
			:required t
			:key :name)
   (clingon:make-option :string
			:description "MAC address of the host"
			:short-name #\a
			:long-name "address"
			:required t
			:key :address)))

(defun add-host/command ()
  "Returns the command for adding hosts to the database file"
  (clingon:make-command
   :name "add-host"
   :description "add host to the database"
   :aliases '("a")
   :options (add-host/options)
   :handler #'add-host/handler))
