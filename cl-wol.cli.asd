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

(defpackage :cl-wol-cli-system
  (:use :cl :asdf))
(in-package :cl-wol-cli-system)

(defsystem "cl-wol.cli"
  :name "cl-wol.cli"
  :long-name "cl-wol.cli"
  :description "CLI built on top of the cl-wol.core system"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :long-description #.(uiop:read-file-string
		       (uiop:subpathname *load-pathname* "README.org"))
  :homepage "https://github.com/dnaeon/cl-wol"
  :bug-tracker "https://github.com/dnaeon/cl-wol"
  :source-control "https://github.com/dnaeon/cl-wol"
  :depends-on (:cl-wol.core
	       :clingon
	       :cl-migratum
	       :cl-migratum.driver.sql
	       :cl-migratum.provider.local-path)
  :build-operation "program-op"
  :build-pathname "bin/wol"
  :entry-point "cl-wol.cli:main"
  :components ((:module "migrations"
		:pathname #P"src/cli/migrations/"
		:components ((:static-file "20211222183337-add_hosts_table.down.sql")
			     (:static-file "20211222183337-add_hosts_table.up.sql")))
	       (:module "cli"
		:pathname #P"src/cli/"
		:depends-on ("migrations")
		:serial t
		:components ((:file "package")
			     (:file "wake")
			     (:file "zsh-completions")
			     (:file "print-doc")
			     (:file "init-db")
			     (:file "main")))))
