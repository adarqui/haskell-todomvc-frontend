build:
	stack build --fast

clean:
	stack clean

test:
	stack test

ghcjs:
	until stack install --stack-yaml=stack.ghcjs.yaml; do echo trying again; done
