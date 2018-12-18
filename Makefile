.PHONY: default
default: build

.PHONY: build
build: ## Build the source
	dune build @install @examples

.PHONY: test
test: ## Run tests
	dune runtest --force

.PHONY: format
format: ## Run OCamlformat on the source
	dune build @fmt --auto-promote

.PHONY: clean
clean: ## Clean the source tree
	dune clean

.PHONY: distrib
distrib: ## Create a distribution tarball
	dune-release distrib

.PHONY: tag
tag: ## Tag the current release
	dune-release tag

.PHONY: publish
publish: ## Put the release on GitHub
	dune-release publish distrib

.PHONY: promote-ppx-output
promote-ppx-output: MAKEFILE_DIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))
promote-ppx-output: ## Promotes the current output of the ppx unit tests to be the new expected output.
	dune runtest --force || true # Without the 'true' Make will abort.
	cp $(MAKEFILE_DIR)_build/default/tests/test_ppx/test_ppx.result.reformatted.ml \
	   $(MAKEFILE_DIR)tests/test_ppx/test_ppx.expected.ml

.PHONY: help
help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
