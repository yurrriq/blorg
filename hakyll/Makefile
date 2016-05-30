# N.B. This requires Gnu Make >= 3.82 for ONESHELL to work as expected.

SITE_DIR := $(addprefix $(CURDIR), /_site)
SITE      = @stack exec site
EMACS     = @emacsclient -e
HACKS     = {.git,CNAME,README.org}
blorg.el := $(addprefix $(dir $(CURDIR)), src/blorg.el)


build: ; $(SITE) build

clean: stash-submodule
	$(SITE) clean

.ONESHELL:
commit-site:
	@cd $(SITE_DIR)
	@git checkout gh-pages
	@git add .
	@git commit -S -m 'Update site'
	@git push
	@cd ..
	@git add .
	@git commit -S -m 'Update hakyll/_site'

deploy: org-publish-project clean rebuild commit-site

org-setup: ; $(EMACS) '(load-file "$(blorg.el)")'

org-publish-project: org-setup
	$(EMACS) '(org-publish-project "blorg")'

rebuild: clean do-rebuild restore-submodule

do-rebuild:
	@stack build
	$(SITE) rebuild

restore-submodule:
	@mv _site-dot-git/$(HACKS) $(SITE_DIR)
	@rm -r _site-dot-git

stash-submodule:
	@mkdir -p _site-dot-git
	@mv $(SITE_DIR)/$(HACKS) _site-dot-git
