server: build
	stack exec bodyweight-server-exe

watch: install
	stack -j4 build --pedantic --file-watch

clean:
	stack clean

maintainer-clean:
	stack clean --full


build: install ./src/** ./app/** bodyweight-server.cabal Setup.hs
	stack -j4 build --pedantic

install: bodyweight-server.cabal
	stack -j4 install --only-dependencies
