build: elm.js

elm.js: Main.elm Schema.elm
	elm make Main.elm --output=$@

###

NODE_URL = http://fred.yankowski.com:8732

dist: site site/elm.js site/index.html site/tezos.css

site:
	mkdir site

site/elm.js: elm.js
	cp $< $@

site/index.html: index.html Makefile
	perl -pe 's#http://localhost:8732#$(NODE_URL)#' index.html >$@

site/tezos.css: tezos.css
	cp $< $@
