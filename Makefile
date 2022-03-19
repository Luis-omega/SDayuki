SHELL := /usr/bin/env bash

.PHONY: build clean test ghci 

usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available options:"
	@echo "  FLAGS   -- Additional options passed to --ghc-options"
	@echo
	@echo "Available commands:"
	@echo "  build               -- Run cabal v2-build"
	@echo "  clean               -- Run cabal v2-clean"
	@echo "  test                -- Run cabal v2-test"
	@echo "  ghci                -- Run stack ghci"


ifdef FLAGS
GHC_FLAGS = --ghc-options "$(FLAGS)"
endif


build: requires_nix_shell SDayuki.cabal
	cabal v2-build $(GHC_FLAGS)

clean: requires_nix_shell SDayuki.cabal
	cabal v2-clean

test: requires_nix_shell SDayuki.cabal
	cabal v2-test --test-show-details=direct

ghci: requires_nix_shell SDayuki.cabal
	cabal v2-repl $(GHC_FLAGS)


# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ -v IN_NIX_SHELL ] || echo "The $(MAKECMDGOALS) target must be run from inside nix-shell"
	@ [ -v IN_NIX_SHELL ] || (echo "    run 'nix-shell --pure' first" && false)


