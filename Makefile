build: elm.js

elm.js: Main.elm Schema.elm
	elm make Main.elm --output=$@
