# NOTE:
#
# This Makefile is very much tailored to the maintainer's environment.
# It might work for you, but don't expect much.


NIXPKGS := $(shell nix-build -Q --no-out-link ./nix/fetch-nixpkgs.nix 2>/dev/null)

nix-build-attr = nix-build --no-out-link nix/jobsets/release.nix -I nixpkgs=$(NIXPKGS) -A $(1)

nix-build = nix-build --no-out-link nix/jobsets/release.nix -I nixpkgs=$(NIXPKGS)

fm-assistant:	nix
		nix-build --no-out-link nix/jobsets/testing.nix -I nixpkgs=$(NIXPKGS) -A fm-assistant

nixpkgs:	nix
		$(call nix-build-attr,nixpkgs)

lts-%:	nix
	$(call nix-build-attr,lts-$*)

release: nix
	 $(call nix-build)

# Note: does not depend on nixpkgs.
next:	nix
	nix-build --no-out-link nix/jobsets/next.nix

doc:	test
	@echo "*** Generating docs"
	cabal haddock --hyperlink-source

test:	build
	@echo "*** Running tests"
	cabal test

help:
	@echo "Targets:"
	@echo
	@echo "Cabal/Nix:"
	@echo
	@echo "(Default is 'fm-assistant')"
	@echo
	@echo "The following targets assume that you are running Nix with some version"
	@echo "of cabal and GHC in your environment."
	@echo
	@echo "    fm-assistant - build fm-assistant against nixpkgs using nix-build (quick)"
	@echo "    nixpkgs   	- build fm-assistant against nixpkgs using nix-build"
	@echo "    release   	- Run nix-build on all release.nix targets"
	@echo
	@echo "    test      	- configure and build the package, then run the tests"
	@echo "    build     	- configure and build the package"
	@echo "    configure 	- configure the package"
	@echo
	@echo "General:"
	@echo
	@echo "    clean - remove all targets"
	@echo "    help  - show this message"

build:	configure
	@echo "*** Building the package"
	cabal build

sdist:	check doc
	@echo "*** Creating a source distribution"
	cabal sdist

check:
	@echo "*** Checking the package for errors"
	cabal check

configure: nix fm-assistant.cabal
	@echo "*** Configuring the package"
	cabal configure -f test-hlint

nix: 	fm-assistant.cabal
	@echo "*** Generating pkgs/fm-assistant.nix"
	cd nix/pkgs && cabal2nix --flag test-hlint ../../. > fm-assistant.nix

fm-assistant.cabal: package.yaml
	@echo "*** Running hpack"
	hpack

clean:
	cabal clean

.PHONY: clean nix
