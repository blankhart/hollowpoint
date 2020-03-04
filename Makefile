
# Run the example
.PHONY: example
example :
	rm -rf example/output/Example && \
	cabal run hollowpoint compile example/*.purs -- -o example/output

# && bat example/output/Example/index.js

