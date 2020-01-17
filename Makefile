.PHONY: all run clean

all: Hibachi.cabal
	cabal new-build

run: Hibachi.cabal
	cabal new-run hibachi

clean: Hibachi.cabal
	cabal new-clean

Hibachi.cabal: package.yaml
	hpack
