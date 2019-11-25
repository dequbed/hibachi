all: Hibachi.cabal
	cabal new-build

Hibachi.cabal: package.yaml
	hpack
