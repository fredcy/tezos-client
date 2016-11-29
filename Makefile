build: elm.js

elm.js: Main.elm
	elm make Main.elm --output=$@
