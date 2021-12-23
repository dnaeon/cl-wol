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

(defun db-migrations-path ()
  "Returns the path to the migration files"
  (asdf:system-relative-pathname :cl-wol.cli "src/cli/migrations/"))

(defun make-db-conn (db-path)
  "Creates a new database connection to the given DB-PATH"
  (cl-dbi:connect :sqlite3 :database-name db-path))

(defun disconnect-db-conn (db-conn)
  "Disconnects from the database"
  (cl-dbi:disconnect db-conn))

(defun migrate-db (db-conn)
  "Migrates the database to the latest version"
  (let* ((provider (cl-migratum.provider.local-path:make-local-path-provider (db-migrations-path)))
	 (driver (cl-migratum.driver.sql:make-sql-driver provider db-conn)))
    (cl-migratum:provider-init provider)
    (cl-migratum:driver-init driver)
    (cl-migratum:apply-pending driver)
    (cl-migratum:provider-shutdown provider)
    (cl-migratum:driver-shutdown driver)))
