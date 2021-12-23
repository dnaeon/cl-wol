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

(defun delete-host/handler (cmd)
  "Handler for the `delete-host' command"
  (unless (clingon:command-arguments cmd)
    (clingon:print-usage-and-exit cmd t))
  (let* ((database (clingon:getopt cmd :database))
	 (db-conn (make-db-conn database)))
    (dolist (name (clingon:command-arguments cmd))
      (db-execute db-conn "DELETE FROM hosts WHERE name = ?" name))))

(defun delete-host/options ()
  "Returns the options of the `delete-host' command"
  (list
   (clingon:make-option :filepath
			:description "path to the database file"
			:short-name #\d
			:long-name "database"
			:env-vars '("DATABASE")
			:required t
			:key :database)))

(defun delete-host/command ()
  "Returns the command for deleting hosts from the database file"
  (clingon:make-command
   :name "delete-host"
   :description "delete host(s) from the database"
   :usage "NAME ..."
   :options (delete-host/options)
   :handler #'delete-host/handler))
