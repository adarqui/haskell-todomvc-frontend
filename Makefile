build:
	stack build --fast

clean:
	stack clean

test:
	stack test

docs:
	stack build --haddock haskell-todomvc-frontend

ghcjs:
	until stack install --stack-yaml=stack.ghcjs.yaml --copy-bins --local-bin-path=./dist/react-flux; do echo trying again; done
	find . -name "*reactflux.jsexe" -exec rsync -av {}/ ./dist/react-flux/ \;
	ccjs ./dist/react-flux/all.js --compilation_level=ADVANCED_OPTIMIZATIONS > ./dist/react-flux/all.min.js

ghci:
	stack ghci haskell-todomvc-frontend

copy-to-backends:
	rsync -av dist/react-flux/ ../haskell-todomvc-backend/static/react-flux/
