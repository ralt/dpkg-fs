ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SOURCES := $(wildcard src/*.lisp) $(wildcard *.asd) $(wildcard t/*.lisp)
APP_NAME=dpkg-fs

.PHONY: dist manual

$(APP_NAME): quicklisp-manifest.txt $(SOURCES)
	@buildapp  --manifest-file quicklisp-manifest.txt \
		--eval '(push "$(ROOT_DIR)/" asdf:*central-registry*)' \
		--load-system $(APP_NAME) \
		--eval '($(APP_NAME):disable-debugger)' \
		--compress-core \
		--output $(APP_NAME) --entry $(APP_NAME):main

quicklisp-manifest.txt:
	@sbcl --non-interactive \
		--eval '(push #P"$(ROOT_DIR)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :$(APP_NAME))' \
		--eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")'

manual:
	@mkdir -p dist/
	@pandoc -s -t man docs/$(APP_NAME).md > dist/$(APP_NAME).1
	@gzip dist/$(APP_NAME).1

dist:
	@make $(APP_NAME)
	@make manual
	@mkdir -p dist/
	@cp $(APP_NAME) dist/
	@./$(APP_NAME) package
	@mv $(APP_NAME)_*_amd64.deb dist/
	@rm -f dist/$(APP_NAME) dist/$(APP_NAME).1.gz
