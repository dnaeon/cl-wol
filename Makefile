.DEFAULT_GOAL := cli
LISP ?= sbcl

test:
	${LISP} --eval '(ql:quickload :cl-wol.test)' \
		--eval '(setf rove:*enable-colors* nil)' \
		--eval '(asdf:test-system :cl-wol.test)' \
		--eval '(quit)'

cli:
	${LISP} --eval '(ql:quickload :cl-wol.cli)' \
		--eval '(asdf:make :cl-wol.cli)' \
		--eval '(quit)'

doc: cli
	./bin/wol print-doc > docs/wol-cli.md

.PHONY: cli doc test
