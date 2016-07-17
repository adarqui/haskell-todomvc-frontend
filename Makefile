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
