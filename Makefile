blorg := nix run ".\#blorg" --

.PHONY: all build clean  \
org-publish-project rebuild do-rebuild

all: org-publish-project rebuild

build: ; ${blorg} build

clean: ; ${blorg} clean

org-publish-project:
	emacs --batch --quick \
		--load emacs.el \
		--eval '(org-publish-project "blorg")'

rebuild: clean do-rebuild

do-rebuild: ; ${blorg} rebuild

fix-clojure: docs/2015/07/ip.ericb.me/index.html docs/404.html
	sed -i 's///' $^
