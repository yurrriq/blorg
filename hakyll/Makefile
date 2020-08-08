# N.B. This requires Gnu Make >= 3.82 for ONESHELL to work as expected.

SITE_DIR := $(addprefix $(CURDIR), /_site)
SITE_TMP := $(shell mktemp -d)
SITE     := site
EMACS     = @ emacsclient -e
HACKS     = {.git,CNAME,README.org}
blorg.el := $(addprefix $(dir $(CURDIR)), src/blorg.el)


.PHONY: build clean site commit-site deploy \
org-setup org-publish-project rebuild do-rebuild restore-submodule stash-submodule


build: ; ${SITE} build


clean: stash-submodule
	${SITE} clean
	@ rm -fr dist


.ONESHELL:
commit-site:
	@ cd ${SITE_DIR}
	@ git checkout gh-pages
	@ git add .
	@ git commit -S -m 'Update site'
	@ echo git push
	@ cd ..
	@ git add .
	@ git commit -S -m 'Update hakyll/_site'


deploy: org-publish-project rebuild commit-site


org-setup: ; ${EMACS} '(load-file "${blorg.el}")'


org-publish-project: org-setup
	${EMACS} '(org-publish-project "blorg")'


rebuild: clean do-rebuild restore-submodule


do-rebuild:
	${SITE} rebuild


restore-submodule:
	@ mv ${SITE_TMP}/${HACKS} ${SITE_DIR}
	@ rm -r ${SITE_TMP}


stash-submodule:
	@ mkdir -p ${SITE_TMP}
	@ mv ${SITE_DIR}/${HACKS} ${SITE_TMP}
	@ echo ${SITE_TMP}


fix-clojure: _site/2015/07/ip.ericb.me/index.html _site/404.html
	sed -i 's///' $^


release.nix: blorg.cabal
	@ cabal2nix --maintainer yurrriq . >$@
