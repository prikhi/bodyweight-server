server: build
	stack exec bodyweight-server-exe

watch: build
	stack build --pedantic --file-watch

clean:
	stack clean

maintainer-clean:
	stack clean --full


build: install ./src/** ./app/** bodyweight-server.cabal Setup.hs
	stack build --pedantic

install: bodyweight-server.cabal
	stack install --only-dependencies
