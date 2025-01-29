# `ontoprepare`: Validating and incrementing versioning

## Preamble
This preparation script has two elements:

1. Increment the version (major, minor or patch-level components) from the latest version.
2. Generate a new `.htaccess` document pointing to that new version, with redirects for the standard semweb content types.

## Assumptions

This script assumes we deploy in a directory with a structure not unlike the following:

`ns/<ontology-base-name>/<version>/<ontology-base-name>.{ttl,rdf,nt}`

as well as `index.html` for HTML documentation at the same level as the RDF serialisations.

The versioning is meaningful, following the [Semantic Versioning](https://semver.org/) scheme. We imagine that changes to an ontology's semantics which break existing, downstream usage, constitute a major change; modifications which add new semantics but don't break existing usage constitute a minor change; and fixes like typos in human-readable RDF/S labels/comments constitute patch-level changes. The script supports incrementing these individually.

The other assumption that we make about versioning is that when incrementing a major or minor version, the lower-level components are reset to 0. This would mean that if the major version of 10.2.99 was incremented, we would end up with 11.0.0. Semantic versioning also supports extra version information after the patch-level numeric identifier, e.g. rc1, and if we extended our scheme to support that, we might reset that too when incrementing the patch version.

## In practice

The script generates an initial `.htaccess` file. The purpose of this is to redirect requests to the latest version, and to rewrite requests so that they go to the requested RDF syntax/serialisation. The script is designed to be called by a makefile, and the assumption is that subsequent, separate stages will generate the serialisations and HTML.

The `.htaccess` file will sit at the same level as the ontology in question, so there is one per ontology. This does duplicate work—since we might in principle want a single, simple `.htaccess` configuration which handles only the content type headers—but for each deployed vocabulary, it lets us generate different rules, formats and pointers to latest versions. 

The rules are that with `text/html`, the human-readable documentation is returned at `index.html`. That's what happens when we use a web browser. The other requests (e.g. `application/n-triples`) point to the right serialisation.

This script is written in Scheme. It is moderately complex, so a shell-scripting language isn't practical, and we imagine working on the RDF directly in future to annotate it with versions. We also imagine working with non-RDF formats. It doesn't write to the filesystem at the moment, but prepares an existing well-formed structure.

## Example usage

Imagine a pre-deployed directory `ns/beam/` with the following version directory structure (note the latest version is actually `4.2.13`):

	0.1.1/	0.1.2/	0.3.1/	0.3.2/	0.3.7/	1.0.0/	1.1.1/	4.2.13/  4.2.2/
	
Increment the major version compared to the latest version found:

	./ontoprepare -I -d ./ns/some-ontology
	
	Options -MultiViews
	AddType text/turtle .ttl
	AddType application/n-triples .nt
	AddType application/rdf+xml .rdf
	RewriteEngine On
	RewriteCond %{HTTP_ACCEPT} text/turtle
	RewriteRule ^some-ontology$ /ns/some-ontology/5.0.0/some-ontology.ttl [R=303,L]
	RewriteCond %{HTTP_ACCEPT} application/n-triples
	RewriteRule ^some-ontology$ /ns/some-ontology/5.0.0/some-ontology.nt [R=303,L]
	RewriteCond %{HTTP_ACCEPT} application/rdf+xml
	RewriteRule ^some-ontology$ /ns/some-ontology/5.0.0/some-ontology.rdf [R=303,L]
	RewriteCond %{HTTP_ACCEPT} text/html
	RewriteRule ^some-ontology$ /ns/some-ontology/5.0.0/index.html [R=303,L]

Increment the minor version:

	% ./ontoprepare -i -d ./ns/some-ontology

	Options -MultiViews
	AddType text/turtle .ttl
	AddType application/n-triples .nt
	AddType application/rdf+xml .rdf
	RewriteEngine On
	RewriteCond %{HTTP_ACCEPT} text/turtle
	RewriteRule ^some-ontology$ /ns/some-ontology/4.3.0/some-ontology.ttl [R=303,L]
	RewriteCond %{HTTP_ACCEPT} application/n-triples
	RewriteRule ^some-ontology$ /ns/some-ontology/4.3.0/some-ontology.nt [R=303,L]
	RewriteCond %{HTTP_ACCEPT} application/rdf+xml
	RewriteRule ^some-ontology$ /ns/some-ontology/4.3.0/some-ontology.rdf [R=303,L]
	RewriteCond %{HTTP_ACCEPT} text/html
	RewriteRule ^some-ontology$ /ns/some-ontology/4.3.0/index.html [R=303,L]

Set the new version arbitrarily:

	% ./ontoprepare -f 32.3.3 -d ./ns/some-ontology

	Options -MultiViews
	AddType text/turtle .ttl
	AddType application/n-triples .nt
	AddType application/rdf+xml .rdf
	RewriteEngine On
	RewriteCond %{HTTP_ACCEPT} text/turtle
	RewriteRule ^some-ontology$ /ns/some-ontology/4.2.14/some-ontology.ttl [R=303,L]
	RewriteCond %{HTTP_ACCEPT} application/n-triples
	RewriteRule ^some-ontology$ /ns/some-ontology/4.2.14/some-ontology.nt [R=303,L]
	RewriteCond %{HTTP_ACCEPT} application/rdf+xml
	RewriteRule ^some-ontology$ /ns/some-ontology/4.2.14/some-ontology.rdf [R=303,L]
	RewriteCond %{HTTP_ACCEPT} text/html
	RewriteRule ^some-ontology$ /ns/some-ontology/4.2.14/index.html [R=303,L]

Sample options:

    ontoprepare [options]
	  -I --increment-major     Increment version major component
	  -i --increment-minor     Increment version minor component
	  -p --increment-patch     Increment version patch-level component
	  -F --force-version VSN   Use VSN as new version (MAJOR.MINOR.PATCH)
	  -d --directory DIR       Use DIR as ontology deployment directory
	  -E --exclude-preamble    Exclude the preamble so that output can be concatenated
	  -V --version-only        Return the new, incremented version string only
	  -L --last-version        Return the last valid version directory found
	  -h --help                Display this help message
