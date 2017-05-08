SRC = $(shell find src -name '*.elm')

build: elm.js

elm.js: $(SRC)
	elm make src/Main.elm --output=$@

###

NODE_URL = https://tezos.ostraca.org

dist: docs docs/elm.js docs/index.html docs/tezos.css

docs:
	mkdir docs

docs/elm.js: elm.js
	cp $< $@

docs/index.html: index.html Makefile
	perl -pe 's#http://localhost:8732#$(NODE_URL)#' index.html >$@

docs/tezos.css: tezos.css
	cp $< $@
