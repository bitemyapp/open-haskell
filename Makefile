all: deploy

CMD=stack exec -- site

cabal-build:
	stack build

clean:
	${CMD} clean

build:
	${CMD} build

deploy: clean build
	${CMD} deploy

watch: clean
	${CMD} watch
