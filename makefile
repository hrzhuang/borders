# default version is dev, override with `make version=1.0`, for example
# used for service worker
version = dev

.PHONY: all clean

all: build/index.html build/style.css build/main.js build/elm.js build/sw.js build/assets

clean:
	rm -rf build

build/index.html: build index.html
	cp index.html build

build/style.css: build style.css
	cp style.css build

build/main.js: build main.js
	cp main.js build

build/elm.js: build src/Main.elm src/Animate.elm src/Countries.elm
	elm make --optimize --output=build/elm.js src/Main.elm

build/sw.js: build sw.js.template
	sed 's/\$${version}/$(version)/g' sw.js.template > build/sw.js

build/assets: build assets
	cp -r assets build

build:
	mkdir build
