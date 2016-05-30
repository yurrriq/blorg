# N.B. This requires Gnu Make >= 3.82 for ONESHELL to work as expected.

SITE_DIR := $(addprefix $(dir $(dir $(dir $(CURDIR)))), blorg-site)
SITE      = @stack exec site
EMACS     = emacsclient -e
blorg.el := $(addprefix $(dir $(CURDIR)), src/blorg.el)


build: ; $(SITE) build

clean: ; $(SITE) clean

.ONESHELL:
commit-site:
	@cd $(SITE_DIR)
	@git add .
	@git commit -S -m 'Update site'
	@git push

copy-site: ; @cp -r $(CURDIR)/_site/* $(SITE_DIR)

deploy: org-setup clean rebuild copy-site commit-site

org-setup: ; $(EMACS) '(load-file "$(blorg.el)")'

org-publish-project: org-setup
	$(EMACS) '(org-publish-project "blorg")'

rebuild: clean
	@stack build
	$(SITE) rebuild
