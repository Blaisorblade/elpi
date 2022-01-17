help:
	@echo 'Known targets:'
	@echo
	@echo '  build            builds elpi'
	@echo '  install          install elpi'
	@echo '  clean            remove build artifacts'
	@echo
	@echo '  tests            runs the entire test suite'
	@echo '  tests ONLY=rex   runs only tests matching rex'
	@echo
	@echo '  git/treeish      checkout treeish and build elpi.git.treeish'
	@echo

INSTALL=_build/install/default
BUILD=_build/default
SHELL:=/usr/bin/env bash
TIMEOUT=90.0
PWD=$(shell pwd)
RUNNERS=\
  dune \
  $(PWD)/$(INSTALL)/bin/elpi \
  $(addprefix $(PWD)/,$(wildcard _build/git/*/$(INSTALL)/bin/elpi.git.*)) \
  $(shell if type tjsim >/dev/null 2>&1; then type -P tjsim; else echo; fi)
TIME=--time $(shell if type -P gtime >/dev/null 2>&1; then type -P gtime; else echo /usr/bin/time; fi)
STACK=1114112
DUNE_OPTS=

CP5:=$(shell ocamlfind query camlp5)

build:
	cp -f src/parser2Messages.messages /tmp/parserMessages.messages.bak
	dune exec menhir -- src/parser2.mly src/tokens.mly --base parser2 \
		--update-errors src/parser2Messages.messages \
		> /tmp/parserMessages.updated
	mv /tmp/parserMessages.updated src/parser2Messages.messages
	dune build $(DUNE_OPTS) @all
	# hack: link camlp5.gramlib in the plugin
	ocamlfind opt -shared -linkall -o _build/install/default/lib/elpi/elpi.cmxs \
		$(CP5)/gramlib.cmxa \
		_build/install/default/lib/elpi/elpi.cmxa


install:
	dune install $(DUNE_OPTS)

doc:
	dune build $(DUNE_OPTS) @doc

clean:
	rm -rf _build

tests:
	$(MAKE) build
	dune runtest
	ulimit -s $(STACK); OCAMLRUNPARAM=l=$(STACK) \
		tests/test.exe \
		--seed $$RANDOM \
		--timeout $(TIMEOUT) \
		$(TIME) \
		--sources=$(PWD)/tests/sources/ \
		--plot=$(PWD)/tests/plot \
		$(addprefix --name-match ,$(ONLY)) \
		$(addprefix --runner , $(RUNNERS))

git/%:
	rm -rf "_build/git/elpi-$*"
	mkdir -p "_build/git/elpi-$*"
	git clone -l . "_build/git/elpi-$*"
	cd "_build/git/elpi-$*" && git checkout "$*"
	cd "_build/git/elpi-$*" && \
	  if [ -f dune ]; then \
	    make build DUNE_OPTS="$(DUNE_OPTS) --root .";\
	    cd _build/install/default/bin/; \
		ln -sf elpi elpi.git.$*; \
	  else \
	    make; \
		mkdir -p _build/install/default/bin/; \
		ln -s $$PWD/elpi _build/install/default/bin/elpi.git.$*; \
	  fi

menhir:
	menhir --interpret src/tokens.mly --base parser2 src/parser2.mly 2>/dev/null
.PHONY: complete
complete:
	menhir src/tokens.mly --base parser2 src/parser2.mly \
		--list-errors > src/parser2Messages.auto.messages
	menhir src/tokens.mly --base parser2 src/parser2.mly\
		--merge-errors src/parser2Messages.auto.messages \
		--merge-errors src/parser2Messages.messages \
		> src/parser2Messages.merged
	mv src/parser2Messages.merged src/parser2Messages.messages
	rm src/parser2Messages.auto.messages

strip:
	sed -e "/^##/d" -i.bak src/parser2Messages.messages

.PHONY: tests help install build clean
