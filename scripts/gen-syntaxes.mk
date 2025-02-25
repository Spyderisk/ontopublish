.ONESHELL:

TITLE		= "My Ontology"
INPUT_BASE	= my-ontology
GRAPH_IDENT	= "http://ontology.spyderisk.org/p/ex"
HIST_IDENT	= "http://ontology.spyderisk.org/v/ex"
DELIM		= "\#"
GRAPH_PREFIX	= "ex"
HIST_PREFIX	= "exv"

SUB_ROOT	= ns
VSN_ROOT	= v

SITE_DIR	= ./site
MD_DIR		= ./docs
MKDOCS_TEMPLATE = ./mkdocs.yaml.in

INPUT_TTL	= $(INPUT_BASE).ttl
EXTRA_N_TRIPLES	= $(INPUT_BASE).nt
EXTRA_RDF_XML	= $(INPUT_BASE).rdf

# PREV_VERSION	= $(shell ontoprepare -L -d $(TARGET_ROOT))
# TARGET_SITE	= $(TARGET_ROOT)/$(TARGET_VERSION)

TARGET_ROOT	= $(SUB_ROOT)/$(INPUT_BASE)
TARGET_HTACCESS	= $(TARGET_ROOT)/.htaccess
TARGET_DOC_CONF	= $(INPUT_BASE).mkdocs.yaml

INITIAL_VERSION = 0.0.1

help:
	@echo "
	Versioning scheme: MAJOR.MINOR.PATCH
	
	make initialise-with VERSION (default 0.0.1)
	make increment-major (increment MAJOR version component)
	make increment-minor (increment MINOR version component)
	make increment-patch (increment PATCH version component)
	"

initialise-with: $(VSN_ROOT)
	TARGET_SITE=$(TARGET_ROOT)/$(INITIAL_VERSION) ;
	mkdir -p $$TARGET_SITE
	ontoprepare -F $(INITIAL_VERSION) -d $(TARGET_ROOT) > $(TARGET_HTACCESS) ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -U $(INPUT_TTL) -V $(INITIAL_VERSION) >  $$TARGET_SITE/$(INPUT_TTL) ;\
	touch ./empty.ttl
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -U ./empty.ttl -V $(INITIAL_VERSION) > v/$(INPUT_TTL) ;
	rm ./empty.ttl
	rapper -i turtle -o ntriples $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_N_TRIPLES) ;
	rapper -i turtle -o ntriples $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_RDF_XML) ;
	rapper -i turtle -o html $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/index.html ;

increment-major: save-old-htaccess
	LAST_VERSION=$$(ontoprepare -L -d $(TARGET_ROOT))
	NEW_VERSION=$$(ontoprepare -V -I -d $(TARGET_ROOT)) ;
	TARGET_SITE=$(TARGET_ROOT)/$$NEW_VERSION/
	mkdir -p $$TARGET_SITE
	ontoprepare -I -d $(TARGET_ROOT) > $(TARGET_HTACCESS) ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -U $(TARGET_ROOT)/$$LAST_VERSION/$(INPUT_TTL) -V $$NEW_VERSION >  $$TARGET_SITE/$(INPUT_TTL) ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -L v/$(INPUT_TTL) -V $$NEW_VERSION > v/$(INPUT_TTL).new ;
	mv v/$(INPUT_TTL).new v/$(INPUT_TTL) ;
	rapper -i turtle -o ntriples $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_N_TRIPLES) ;
	rapper -i turtle -o ntriples $(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_RDF_XML) ;
	rapper -i turtle -o html $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/index.html ;

increment-minor: save-old-htaccess
	LAST_VERSION=$$(ontoprepare -L -d $(TARGET_ROOT))
	NEW_VERSION=$$(ontoprepare -V -i -d $(TARGET_ROOT)) ;
	TARGET_SITE=$(TARGET_ROOT)/$$NEW_VERSION/
	mkdir -p $$TARGET_SITE
	ontoprepare -I -d $(TARGET_ROOT) > $(TARGET_HTACCESS) ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -U $(TARGET_ROOT)/$$LAST_VERSION/$(INPUT_TTL) -V $$NEW_VERSION >  $$TARGET_SITE/$(INPUT_TTL) ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -L v/$(INPUT_TTL) -V $$NEW_VERSION > v/$(INPUT_TTL).new ;
	mv v/$(INPUT_TTL).new v/$(INPUT_TTL) ;
	rapper -i turtle -o ntriples $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_N_TRIPLES) ;
	rapper -i turtle -o ntriples $(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_RDF_XML) ;
	rapper -i turtle -o html $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/index.html ;

increment-patch: save-old-htaccess
	LAST_VERSION=$$(ontoprepare -L -d $(TARGET_ROOT))
	NEW_VERSION=$$(ontoprepare -V -p -d $(TARGET_ROOT)) ;
	TARGET_SITE=$(TARGET_ROOT)/$$NEW_VERSION/
	mkdir -p $$TARGET_SITE
	ontoprepare -I -d $(TARGET_ROOT) > $(TARGET_HTACCESS) ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -U $(TARGET_ROOT)/$$LAST_VERSION/$(INPUT_TTL) -V $$NEW_VERSION >  $$TARGET_SITE/$(INPUT_TTL) ;
	ontoconsume -b $(GRAPH_IDENT) -l $(HIST_IDENT) -d $(DELIM) -p $(GRAPH_PREFIX) -P $(HIST_PREFIX) -L v/$(INPUT_TTL) -V $$NEW_VERSION > v/$(INPUT_TTL).new ;
	mv v/$(INPUT_TTL).new v/$(INPUT_TTL) ;
	rapper -i turtle -o ntriples $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_N_TRIPLES) ;
	rapper -i turtle -o ntriples $(INPUT_TTL) > $$TARGET_SITE/$(EXTRA_RDF_XML) ;
	rapper -i turtle -o html $$TARGET_SITE/$(INPUT_TTL) > $$TARGET_SITE/index.html ;

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

$(VSN_ROOT):
	mkdir -p $@

clean:
	rm -f $(TARGET_DOC_CONF) &&
	rm -rf $(MD_DIR) &&
	rm -f /tmp/${INPUT_BASE}.mkdocs.yaml
	#rm -rf $(SITE_DIR)
	#rm -f $(EXTRA_RDF_XML) &&
	#rm -f /tmp/$(EXTRA_N_TRIPLES) &&
