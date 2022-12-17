.DEFAULT_GOAL := cli
LISP ?= sbcl

test:
	./scripts/run-tests.sh

cli:
	${LISP} --eval '(push #P"/app/" ql:*local-project-directories*)' \
		--eval '(ql:quickload :cl-wol.cli)' \
		--eval '(asdf:make :cl-wol.cli)' \
		--eval '(quit)'

doc: cli
	./bin/wol print-doc > docs/wol-cli.md

.PHONY: cli doc test
