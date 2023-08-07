.PHONY: all clean

all: build/index.html build/main.js build/elm.js

clean:
	rm -rf build

build/index.html: build index.html
	cp index.html build

build/main.js: build main.js
	cp main.js build

build/elm.js: build src/Main.elm src/Animate.elm src/Countries.elm
	elm make --optimize --output=build/elm.js src/Main.elm

build:
	mkdir build
