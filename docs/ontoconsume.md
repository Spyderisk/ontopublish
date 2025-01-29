# `ontoconsume`: Injecting versioning metadata, providing semantic web service annotations

## Preamble

This currently supports two, related behaviours:
    
1. Annotate an RDF document with the *latest* version, with error handling for when we present our utility program with an RDF document which has a more recent one.
2. Annotate an RDF document which records a history of versions (with similar error handling).
    
The next phase will be to provide information about the behaviour of the endpoint which we're building, as this is effectively a pipeline which responds to input, producing output. 
	
## In action
	
Versions in the ontology proper would be declared something like the following. Equally, we would expect to provide this information in a separate file which also records information about the behaviour of the end-point serving these (semantic web services).
    
```turtle
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfg: <http://www.w3.org/2004/03/trix/rdfg-1/> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix send: <http://ontology.spyderisk.org/ns/endpoint#> .
    
<http://ontology.spyderisk.org/ns/core/0.1.0> rdf:type rdfg:Graph ;
    send:has_version _:b1 ;
    send:has_version <http://ontology.spyderisk.org/v/core#0.1.0> ;
    send:has_version_history <http://ontology.spyderisk.org/v/core> .
    
_:b1 a send:Version ;
    send:version_represented_as "0.1.0" ;
    send:major_component "0"^^xsd:integer ;
    send:minor_component "1"^^xsd:integer ;
    send:patch_component "0"^^xsd:integer ;
    send:see_previous <http://ontology.spyderisk.org/v/core#0.0.9> ;
    send:valid_from "2025-01-01"^^xsd:date ;
    send:valid_to "2025-01-16"^^xsd:date ;
    send:applies_to <http://ontology.spyderisk.org/ns/core> .
```
    
The versioning scheme identifiers and relations are currently described using RDF/S in (`examples/versioning.ttl`)[https://github.com/Spyderisk/ontopublish/blob/main/examples/versioning.ttl]. This RDF structure is broadly similar to that described by (Grandi et al. (2012))[https://doi.org/10.1016/j.jbi.2012.07.005]. A recent overview of nominal practice is found in (Garijo & Poveda-Villalón (2020))[https://doi.org/10.48550/arXiv.2003.13084].

Some elements of the versioning scheme are conceptually similar to some of the semantics provided by Owl, but we don't currently use Owl, and we are interested in designing the versioning scheme with respect to the *behaviour* of the endpoint as a publishing pipeline (as well as the history).

We expect this to be served as a vocabulary at `http://ontology.spyderisk.org/ns/endpoint`. Notionally, just as we serve vocabularies from `http://ontology.spyderisk.org/ns/`, we aim to serve corresponding version histories from `http://ontology.spyderisk.org/v/`.
