DOCS_DIR=inst/docs
DOCS=guide.html

DOC_FILES=$(DOCS:%=$(DOCS_DIR)/%)

install: $(DOC_FILES)
	R CMD INSTALL $(INSTALL_ARGS) .

check: 
	R CMD check .

build: $(DOC_FILES)
	(cd .. ; R CMD build odbAccess)

%.html: %.xml
	$(MAKE) -C $(@D) $(@F)

