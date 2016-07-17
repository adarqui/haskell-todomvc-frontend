build:
	stack build --fast

clean:
	stack clean

test:
	stack test

docs:
	stack build --haddock haskell-todomvc-frontend

ghcjs:
	until stack install --stack-yaml=stack.ghcjs.yaml --copy-bins --local-bin-path=./dist; do echo trying again; done
	find . -name "*reactflux.jsexe" -exec rsync -av {}/ ./dist/ \;
	ccjs ./dist/all.js --compilation_level=ADVANCED_OPTIMIZATIONS > ./dist/all.min.js

ghci:
	stack ghci haskell-todomvc-frontend
