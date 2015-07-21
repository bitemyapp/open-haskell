all: deploy

CMD=./dist/build/open-cis194/open-cis194

cabal-build:
	cabal clean
	cabal build

clean:
	${CMD} clean

build: clean
	${CMD} build

deploy: build
	${CMD} deploy

watch: clean
	${CMD} watch
