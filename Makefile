.PHONY: all

all: dist/tsetse.css dist/tsetse.js

dist/tsetse.css: ui/tsetse.scss
	scss $< $@

dist/tsetse.js: ui/pre.js ui/post.js $(wildcard ui/js/*.js)
	cat ui/pre.js ui/js/*.js ui/js/games/*.js ui/post.js > $@
