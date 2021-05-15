.PHONY: all run clean

CABAL_OPTIONS=--ghc-options=-dynamic

all: Hibachi.cabal
	cabal new-build $(CABAL_OPTIONS)

run: Hibachi.cabal
	cabal new-run $(CABAL_OPTIONS) hibachi

clean: Hibachi.cabal
	cabal new-clean

Hibachi.cabal: package.yaml
	hpack
