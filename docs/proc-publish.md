# Publishing RDF

## Preamble

Roughly speaking, our process for publishing goes like this:

1. We have an RDF document for our ontology, vocabulary or instance.
2. We make some changes. These are one of three types:
   1. Non-conservative changes, which break downstream usage. An example of this would be to rename an RDF/S class.
   2. Conservative changes, which make changes to the semantics, but do not break downstream usage. An example of this would be adding a *new* class or properties which does not replace existing classes or properties.
   3. Bug-fixes which do not change the semantics. An example of this is making a change to the human-readable RDF/S labels or comments. Another example of this would be attribution or crediting, or annotations specific to the endpoint serving the RDF or providing a SPARQL service.
3. We have a versioning scheme, which is MAJOR.MINOR.PATCH, where each component is numeric. The three respective changes increment each component respectively. Given a change, or section of changes, we decide which component to change. (These are subsumed/reset, so making a conservative changes as well as non-conservative changes would count as incrementing the major component once.)
4. We use Apache 2 as web server. This is fairly well-established, and we generate `.htaccess` files which should be portable across a number of deployments, where we may not have access to the main web server configuration. We also generate one `.htaccess` file per vocabulary deployment.
5. We have tooling to inject versioning information into RDF documents, as well as appending to a version history. Identifiers link these. The versioning scheme at the RDF level follows (Grandi et al. (2012)[https://doi.org/10.1016/j.jbi.2012.07.005].

## In action

Assuming the following:

1. A local directory.
2. Write access to a webroot, (in this example, `/var/www/htdocs`), with Apache2 configured with (`mod_rewrite`)[https://httpd.apache.org/docs/2.4/rewrite/intro.html]
3. An vocabulary file with a certain base name, e.g. `core.ttl` has the base name `core`. This is important for conversion (and the makefile will be updated to make this behaviour less fragile).

Initialise (no deployed RDF):

	make initialise-with INITIAL_VERSION=0.1.0 INPUT_BASE=core
		
Increment major version:

	make increment-major INPUT_BASE=core

What's going on underneath is a call to our `ontoprepare` script, which knows about the versioning scheme, and generates `.htaccess` files accordingly, pointing at the new version. The script will eventually inject version information into the RDF, as well as information about the endpoint, for external consumption.

The output scheme is `ns/<base>/<version>`. In the first case, we would find `ns/core/0.1.0`, and within it, the input turtle file, conversions to RDF/XML and N Triples, as well as generated HTML documentation.

The top-level of this can be varied from `ns`, using the `SUB_ROOT` environment variable. In the first case, the version forcibly output is `0.1.0`. If we then ran the second case, the existing, deployed version 0.1.0 would be discovered, and then the next version `1.0.0` is derived. When incrementing versions, the lower components are reset to zero, which is why the minor version component got reset to zero.

# Component-specific documentation

- (`ontoprepare`: Validating and incrementing versioning)[https://github.com/Spyderisk/ontopublish/blob/main/docs/ontoprepare.md]
- (`ontoconsume`: Injecting versioning metadata, providing semantic web service annotations)[https://github.com/Spyderisk/ontopublish/blob/main/docs/ontoconsume.md]
