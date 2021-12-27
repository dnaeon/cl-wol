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

(defun list-hosts/handler (cmd)
  "Handler for the `list-hosts' command"
  (let* ((database (clingon:getopt cmd :database))
	 (offset (clingon:getopt cmd :offset))
	 (limit (clingon:getopt cmd :limit))
	 (db-conn (make-db-conn database))
	 (table (ascii-table:make-table '("ID" "NAME" "ADDR" "CREATED AT")))
	 (items (db-execute db-conn "SELECT * FROM hosts LIMIT ? OFFSET ?" limit offset)))
    (dolist (item items)
      (ascii-table:add-row table (list (getf item :|id|)
				       (getf item :|name|)
				       (getf item :|addr|)
				       (getf item :|created_at|))))
    (when items
      (ascii-table:display table))))

(defun list-hosts/options ()
  "Returns the options of the `list-hosts' command"
  (list
   (clingon:make-option :filepath
			:description "path to the database file"
			:short-name #\d
			:long-name "database"
			:env-vars '("DATABASE")
			:required t
			:key :database)
   (clingon:make-option :integer
			:description "offset to use when fetching rows"
			:short-name #\o
			:long-name "offset"
			:initial-value 0
			:key :offset)
   (clingon:make-option :integer
			:description "max number of rows to fetch"
			:short-name #\l
			:long-name "limit"
			:initial-value 20
			:key :limit)))

(defun list-hosts/command ()
  "Returns the command for listing hosts from the database file"
  (clingon:make-command
   :name "list-hosts"
   :description "displays the hosts from the database"
   :options (list-hosts/options)
   :handler #'list-hosts/handler
   :examples '(("List hosts from database:" . "wol list-hosts --database wol.db")
	       ("List 50 hosts at max:" . "wol list-hosts --database wol.db --limit 50"))))
