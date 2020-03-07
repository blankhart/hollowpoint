
# Run the example
.PHONY: example
example :
	rm -rf example/pkg/* && \
	cabal run hollowpoint compile example/src/**/*.purs -- -o example/pkg

# && bat example/output/Example/index.js

