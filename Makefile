
# Run the example
.PHONY: example
example :
	cabal run hollowpoint compile example/*.purs -- -o example/output

