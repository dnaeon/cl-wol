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

(defun top-level/handler (cmd)
  "The top-level handler"
  (clingon:print-usage-and-exit cmd t))

(defun top-level/sub-commands ()
  "Returns the list of top-level sub-commands"
  (list
   (print-doc/command)
   (wake/command)
   (zsh-completions/command)))

(defun top-level/command ()
  "Returns the top-level command"
  (clingon:make-command
   :name "wol"
   :version "0.1.0"
   :description "wake up magic-packet compliant systems"
   :long-description (format nil "The WoL application wakes up ~
                                  remote systems identified by their ~
                                  MAC addresses by broadcasting a ~
                                  magic packet")
   :authors '("Marin Atanasov Nikolov <dnaeon@gmail.com>")
   :license "BSD 2-Clause"
   :handler #'top-level/handler
   :options nil
   :sub-commands (top-level/sub-commands)))

(defun main ()
  "Main CLI entrypoint"
  (let ((app (top-level/command)))
    (clingon:run app)))
