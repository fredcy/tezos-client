SRC = $(shell find src -name '*.elm')

VERSION = $(shell git describe --always)

build: elm.js

elm.js: $(SRC)
	elm make src/Main.elm --output=$@

###

#NODE_URL = https://tezos.ostraca.org
NODE_URL = http://rpc.ostez.com

SITE = site

dist: $(SITE) $(SITE)/elm.js $(SITE)/index.html $(SITE)/tezos.css $(SITE)/tezos.js versionfile

$(SITE):
	mkdir $(SITE)

$(SITE)/elm.js: elm.js
	cp $< $@

$(SITE)/index.html: index.html Makefile
	perl -pe 's#http://localhost:8732#$(NODE_URL)#' index.html >$@

$(SITE)/tezos.css: tezos.css
	cp $< $@

$(SITE)/tezos.js: tezos.js
	perl -pe 's#http://localhost:8732#$(NODE_URL)#' tezos.js >$@

versionfile:
	echo "$(VERSION)" > $(SITE)/version.html

publish: dist
	rsync -av $(SITE)/ fred@a.ostraca.org:explorer/www
