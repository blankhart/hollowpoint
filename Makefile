
# Run the example
.PHONY: example
example :
	rm -rf example/pkg && \
	mkdir example/pkg && \
	cabal run hollowpoint compile std/prelude/**/*.purs -- -o example/pkg && \
	echo "name: pkg" >> example/pkg/pubspec.yaml && \
	cd example/pkg && pub get

# && bat example/output/Example/index.js

