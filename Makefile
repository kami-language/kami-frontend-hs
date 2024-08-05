
agda-sources:
	cd kami/kami-core && agda --compile --ghc-dont-call-ghc --compile-dir=../../src src/KamiCore/Pipeline/Main.agda

build: agda-sources
	hpack
	cabal build

test: build
	cabal test

.PHONY: agda-sources build test

