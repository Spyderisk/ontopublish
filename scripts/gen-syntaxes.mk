.ONESHELL:

MD_DIR		= docs
TITLE		= "BEAM ontology: Ontology of the Erlang virtual machine"
INPUT_RDF	= beam.ttl

.PHONY: prep-mkdocs gen-md gen-site

gen-site: gen-md
	mkdocs build

gen-md: $(MD_DIR) prep-mkdocs
	ontospy gendocs --type 3 --title $(TITLE) -o $(MD_DIR) $(INPUT_RDF)

prep-mkdocs:
	cat << EOF > mkdocs.yaml
	site_name: BEAM Ontology
	theme:
		name: material
	EOF

$(MD_DIR):
	mkdir -p $@

clean:
	rm -rf $(MD_DIR)
	rm -rf site
