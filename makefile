.PHONY: all clean

all: build/index.html build/style.css build/main.js build/elm.js build/assets build/favicon.ico

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

build/assets: build assets
	cp -r assets build

build/favicon.ico: build favicon.ico
	cp favicon.ico build

build:
	mkdir build
