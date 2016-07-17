build:
	stack build --fast

clean:
	stack clean

test:
	stack test

ghcjs:
	until stack install --stack-yaml=stack.ghcjs.yaml --copy-bins --haddock; do echo trying again; done
