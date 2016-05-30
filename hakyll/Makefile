# N.B. This requires Gnu Make >= 3.82 for ONESHELL to work as expected.

SITE_DIR= $(addprefix $(dir $(PWD)), blorg-site)
SITE    = @stack exec site

build: ; $(SITE) build

clean: ; $(SITE) clean

.ONESHELL:
deploy: build
	@cp -r _site/* $(SITE_DIR)
	@cd $(SITE_DIR)
	@git add .
	@git commit -S -m 'Update site'
	@git push

rebuild:
	@stack build
	$(SITE) rebuild
