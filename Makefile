.PHONY: default
default: build

.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune runtest -f

.PHONY: install
install:
	dune install

.PHONY: uninstall
uninstall:
	dune uninstall

.PHONY: clean
clean:
	dune clean
