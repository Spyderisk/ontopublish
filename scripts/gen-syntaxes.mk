.ONESHELL:

TITLE		= "BEAM ontology: Ontology of the Erlang virtual machine"
INPUT_BASE	= beam

# 1. Start by looking in the webroot for existence of .htaccess file
# 2. Get the previous version (-L option of ontoprepare). If the .htaccess file exists, copy it to <prev version>.htaccess and put it somewhere in the source directory of ontologies.
# 3. Append to the file.
# 4. Run the rest of the rules.

# Source directory of ontologies under /srv
# Web root at /var/www/htdocs or similar
# Targeting vocabularies living at ontology.spyderisk.org/ns/
# Perhaps including instances data living at ontology.spyderisk.org/p/

SUB_ROOT	= ns

SITE_DIR	= /tmp/site
MD_DIR		= /tmp/docs

INPUT_TTL	= $(INPUT_BASE).ttl
EXTRA_N_TRIPLES	= $(INPUT_BASE).nt
EXTRA_RDF_XML	= $(INPUT_BASE).rdf

# PREV_VERSION	= $(shell ./ontoprepare.scm -L -d $(TARGET_ROOT))
# TARGET_SITE	= $(TARGET_ROOT)/$(TARGET_VERSION)

TARGET_ROOT	= $(SUB_ROOT)/$(INPUT_BASE)
TARGET_HTACCESS	= $(TARGET_ROOT)/.htaccess
TARGET_DOC_CONF	= $(INPUT_BASE).mkdocs.yaml

increment-major:	html-components rdf-components coalesce-with-new-major
increment-minor:	html-components rdf-components coalesce-with-new-minor
increment-patch:	html-components rdf-components coalesce-with-new-patch

html-components:	gen-md gen-site #prep-mkdocs gen-md gen-site
rdf-components:		gen-syntaxes

gen-syntaxes: $(TARGET_ROOT)
	rapper -i turtle -o ntriples $(INPUT_TTL) > /tmp/$(EXTRA_N_TRIPLES)	&&
	rapper -i turtle -o rdfxml   $(INPUT_TTL) > /tmp/$(EXTRA_RDF_XML)

gen-site: gen-md
	mkdocs build -f /tmp/$(TARGET_DOC_CONF) -d /tmp/site

gen-md: $(MD_DIR) # prep-mkdocs
	ontospy gendocs --type 3 --title $(TITLE) -o /tmp/docs $(INPUT_TTL)

# Start by moving /tmp/site to ./ns/beam/1.0.0 or whatever
coalesce-with-new-major: save-old-htaccess
	NEW_VERSION=$$(./ontoprepare.scm -V -I -d $(TARGET_ROOT)) ;
	TARGET_SITE=$(TARGET_ROOT)/$$NEW_VERSION/
	./ontoprepare.scm -I -d $(TARGET_ROOT) > $(TARGET_HTACCESS) ;

	mv -v /tmp/site $$TARGET_SITE
	cp -v $(INPUT_TTL) $$TARGET_SITE
	mv -v /tmp/$(EXTRA_N_TRIPLES) $$TARGET_SITE
	mv -v /tmp/$(EXTRA_RDF_XML) $$TARGET_SITE

coalesce-with-new-minor: save-old-htaccess
	NEW_VERSION=$$(./ontoprepare.scm -V -i -d $(TARGET_ROOT)) ;
	TARGET_SITE=$(TARGET_ROOT)/$$NEW_VERSION/
	./ontoprepare.scm -i -d $(TARGET_ROOT) > $(TARGET_HTACCESS) ;

	mv -v /tmp/site $$TARGET_SITE
	cp -v $(INPUT_TTL) $$TARGET_SITE
	mv -v /tmp/$(EXTRA_N_TRIPLES) $$TARGET_SITE
	mv -v /tmp/$(EXTRA_RDF_XML) $$TARGET_SITE

coalesce-with-new-patch: save-old-htaccess
	NEW_VERSION=$$(./ontoprepare.scm -V -p -d $(TARGET_ROOT)) ;
	TARGET_SITE=$(TARGET_ROOT)/$$NEW_VERSION/
	./ontoprepare.scm -p -d $(TARGET_ROOT) > $(TARGET_HTACCESS) ;

	mv -v /tmp/site $$TARGET_SITE
	cp -v $(INPUT_TTL) $$TARGET_SITE
	mv -v /tmp/$(EXTRA_N_TRIPLES) $$TARGET_SITE
	mv -v /tmp/$(EXTRA_RDF_XML) $$TARGET_SITE

save-old-htaccess:
	if [ -f $(TARGET_HTACCESS) ]; then
		ORIGINAL_VERSION=$$(./ontoprepare.scm -L -d $(TARGET_ROOT))
		mv -f $(TARGET_HTACCESS) $(INPUT_BASE).$(ORIGINAL_VERSION).htaccess.old
	fi

define tab "  "
endef

prep-mkdocs:
	cat << EOF > /tmp/$(INPUT_BASE).mkdocs.yaml
	site_name: $(TITLE)
	docs_dir:  /tmp/docs
	theme:
	${tab}name: material
	EOF

$(SITE_DIR):
	mkdir -p $@

$(MD_DIR):
	mkdir -p $@

$(TARGET_ROOT):
	mkdir -p $@

clean:
	rm  -f $(TARGET_DOC_CONF) &&
	rm -f /tmp/$(EXTRA_RDF_XML) &&
	rm -f /tmp/$(EXTRA_N_TRIPLES) &&
	rm -rf $(MD_DIR) &&
	rm -rf $(SITE_DIR)

