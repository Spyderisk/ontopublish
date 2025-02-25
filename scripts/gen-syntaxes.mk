.ONESHELL:

TITLE		= "My Ontology"
INPUT_BASE	= my-ontology
GRAPH_IDENT	= "http://ontology.spyderisk.org/p/ex"
HIST_IDENT	= "http://ontology.spyderisk.org/v/ex"
DELIM		= "\#"
GRAPH_PREFIX	= "ex"
HIST_PREFIX	= "ev"

# 1. Start by looking in the webroot for existence of .htaccess file
# 2. Get the previous version (-L option of ontoprepare). If the .htaccess file exists, copy it to <prev version>.htaccess and put it somewhere in the source directory of ontologies.
# 3. Append to the file.
# 4. Run the rest of the rules.

# Source directory of ontologies under /srv
# Web root at /var/www/htdocs or similar
# Targeting vocabularies living at ontology.spyderisk.org/ns/
# Perhaps including instances data living at ontology.spyderisk.org/p/

SUB_ROOT	= ns
SHEX_ROOT	= sh
VSN_ROOT	= v

SITE_DIR	= ./site
MD_DIR		= ./docs
MKDOCS_TEMPLATE = ./mkdocs.yaml.in

INPUT_TTL	= $(INPUT_BASE).ttl
EXTRA_N_TRIPLES	= $(INPUT_BASE).nt
EXTRA_RDF_XML	= $(INPUT_BASE).rdf
EXTRA_SHEXC	= $(INPUT_BASE).shex

# PREV_VERSION	= $(shell ontoprepare -L -d $(TARGET_ROOT))
# TARGET_SITE	= $(TARGET_ROOT)/$(TARGET_VERSION)

TARGET_ROOT	= $(SUB_ROOT)/$(INPUT_BASE)
TARGET_HTACCESS	= $(TARGET_ROOT)/.htaccess
TARGET_DOC_CONF	= $(INPUT_BASE).mkdocs.yaml

TARGET_ROOT_SH = $(SHEX_ROOT)/$(INPUT_BASE)
TARGET_HTACCESS_SH = $(TARGET_ROOT_SH)/.htaccess

INITIAL_VERSION = 1.0.0	

help:
	@echo "
	Versioning scheme: MAJOR.MINOR.PATCH
	
	make initialise-with VERSION (default 1.0.0)
	make increment-major (increment MAJOR version component)
	make increment-minor (increment MINOR version component)
	make increment-patch (increment PATCH version component)
	"

initialise-with: $(TARGET_ROOT) $(TARGET_ROOT_SH) $(VSN_ROOT)
	TARGET_SITE=$(TARGET_ROOT)/$(INITIAL_VERSION) ;
	TARGET_SITE_SH=$(TARGET_ROOT_SH)/$(INITIAL_VERSION) ;
	ontoprepare -F $(INITIAL_VERSION) -d $(TARGET_ROOT) > $(TARGET_HTACCESS) ;
	ontoprepare -F $(INITIAL_VERSION) -d $(TARGET_ROOT_SH) > $(TARGET_HTACCESS_SH) ;
	mkdir -p $$TARGET_SITE $$TARGET_SITE_SH ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -U $(INPUT_TTL) -V $(INITIAL_VERSION) >  $$TARGET_SITE/$(INPUT_TTL) ;\
	touch ./empty.ttl ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -U ./empty.ttl -V $(INITIAL_VERSION) > v/$(INPUT_TTL) ;
	rm ./empty.ttl ;
	rapper -i turtle -o ntriples $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_N_TRIPLES) ;
	rapper -i turtle -o rdfxml $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_RDF_XML) ;
	rapper -i turtle -o html $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/index.html ;
	rapper -i turtle -o ntriples v/$(INPUT_TTL) > v/$(INPUT_BASE).nt ;
	rapper -i turtle -o rdfxml v/$(INPUT_TTL) > v/$(INPUT_BASE).rdf ;
	rapper -i turtle -o html v/$(INPUT_TTL) > v/$(INPUT_BASE).html ;
	cp $(EXTRA_SHEXC) $$TARGET_SITE_SH ;

increment-major: save-old-htaccess
	LAST_VERSION=$$(ontoprepare -L -d $(TARGET_ROOT))
	NEW_VERSION=$$(ontoprepare -V -I -d $(TARGET_ROOT)) ;
	TARGET_SITE=$(TARGET_ROOT)/$$NEW_VERSION ;
	TARGET_SITE_SH=$(TARGET_ROOT_SH)/$$NEW_VERSION ;
	ontoprepare -I -d $(TARGET_ROOT) > $(TARGET_HTACCESS) ;
	ontoprepare -I -d $(TARGET_ROOT_SH) > $(TARGET_HTACCESS_SH) ;
	mkdir -p $$TARGET_SITE $$TARGET_SITE_SH ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -U $(TARGET_ROOT)/$$LAST_VERSION/$(INPUT_TTL) -V $$NEW_VERSION >  $$TARGET_SITE/$(INPUT_TTL) ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -L v/$(INPUT_TTL) -V $$NEW_VERSION > v/$(INPUT_TTL).new ;
	rapper -i turtle -o ntriples $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_N_TRIPLES) ;
	rapper -i turtle -o rdfxml $(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_RDF_XML) ;
	rapper -i turtle -o html $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/index.html ;
	rapper -i turtle -o ntriples v/$(INPUT_TTL).new > v/$(INPUT_BASE).nt ;
	rapper -i turtle -o rdfxml v/$(INPUT_TTL).new > v/$(INPUT_BASE).rdf ;
	rapper -i turtle -o html v/$(INPUT_TTL).new > v/$(INPUT_BASE).html ;
	mv v/$(INPUT_TTL).new v/$(INPUT_TTL) ;
	cp $(EXTRA_SHEXC) $$TARGET_SITE_SH ;

increment-minor: save-old-htaccess
	LAST_VERSION=$$(ontoprepare -L -d $(TARGET_ROOT))
	NEW_VERSION=$$(ontoprepare -V -i -d $(TARGET_ROOT)) ;
	TARGET_SITE=$(TARGET_ROOT)/$$NEW_VERSION ;
	TARGET_SITE_SH=$(TARGET_ROOT_SH)/$$NEW_VERSION ;
	ontoprepare -i -d $(TARGET_ROOT) > $(TARGET_HTACCESS) ;
	ontoprepare -i -d $(TARGET_ROOT_SH) > $(TARGET_HTACCESS_SH) ;
	mkdir -p $$TARGET_SITE $$TARGET_SITE_SH ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -U $(TARGET_ROOT)/$$LAST_VERSION/$(INPUT_TTL) -V $$NEW_VERSION >  $$TARGET_SITE/$(INPUT_TTL) ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -L v/$(INPUT_TTL) -V $$NEW_VERSION > v/$(INPUT_TTL).new ;
	rapper -i turtle -o ntriples $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_N_TRIPLES) ;
	rapper -i turtle -o rdfxml $(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_RDF_XML) ;
	rapper -i turtle -o html $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/index.html ;
	rapper -i turtle -o ntriples v/$(INPUT_TTL).new > v/$(INPUT_BASE).nt ;
	rapper -i turtle -o rdfxml v/$(INPUT_TTL).new > v/$(INPUT_BASE).rdf ;
	rapper -i turtle -o html v/$(INPUT_TTL).new > v/$(INPUT_BASE).html ;
	mv v/$(INPUT_TTL).new v/$(INPUT_TTL) ;
	cp $(EXTRA_SHEXC) $$TARGET_SITE_SH ;

increment-patch: save-old-htaccess
	LAST_VERSION=$$(ontoprepare -L -d $(TARGET_ROOT))
	NEW_VERSION=$$(ontoprepare -V -p -d $(TARGET_ROOT)) ;
	TARGET_SITE=$(TARGET_ROOT)/$$NEW_VERSION ;
	TARGET_SITE_SH=$(TARGET_ROOT_SH)/$$NEW_VERSION ;
	ontoprepare -p -d $(TARGET_ROOT) > $(TARGET_HTACCESS) ;
	ontoprepare -p -d $(TARGET_ROOT_SH) > $(TARGET_HTACCESS_SH) ;
	mkdir -p $$TARGET_SITE $$TARGET_SITE_SH ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -U $(TARGET_ROOT)/$$LAST_VERSION/$(INPUT_TTL) -V $$NEW_VERSION >  $$TARGET_SITE/$(INPUT_TTL) ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -L v/$(INPUT_TTL) -V $$NEW_VERSION > v/$(INPUT_TTL).new ;
	rapper -i turtle -o ntriples $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_N_TRIPLES) ;
	rapper -i turtle -o rdfxml $(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_RDF_XML) ;
	rapper -i turtle -o html $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/index.html ;
	rapper -i turtle -o ntriples v/$(INPUT_TTL).new > v/$(INPUT_BASE).nt ;
	rapper -i turtle -o rdfxml v/$(INPUT_TTL).new > v/$(INPUT_BASE).rdf ;
	rapper -i turtle -o html v/$(INPUT_TTL).new > v/$(INPUT_BASE).html ;
	mv v/$(INPUT_TTL).new v/$(INPUT_TTL) ;
	cp $(EXTRA_SHEXC) $$TARGET_SITE_SH ;

save-old-htaccess:
	if [ -f $(TARGET_HTACCESS) ]; then
		ORIGINAL_VERSION=$$(ontoprepare -L -d $(TARGET_ROOT))
		mv -f $(TARGET_HTACCESS) $(INPUT_BASE).$(ORIGINAL_VERSION).htaccess.old
	fi

prep-mkdocs:
	cat $(MKDOCS_TEMPLATE) | sed s/TITLE/"${TITLE}"/ > /tmp/${INPUT_BASE}.mkdocs.yaml

$(SITE_DIR):
	mkdir -p $@

$(MD_DIR):
	mkdir -p $@

$(TARGET_ROOT):
	mkdir -p $@

$(TARGET_ROOT_SH):
	mkdir -p $@

# No redirects here, need to create the upper level
$(VSN_ROOT):
	mkdir -p $@

clean:
	rm -f $(TARGET_DOC_CONF) &&
	rm -rf $(MD_DIR) &&
	rm -f /tmp/${INPUT_BASE}.mkdocs.yaml
	#rm -rf $(SITE_DIR)
	#rm -f $(EXTRA_RDF_XML) &&
	#rm -f /tmp/$(EXTRA_N_TRIPLES) &&
