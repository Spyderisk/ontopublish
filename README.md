# Ontopublish project

Ontopublish is an effort to publish an RDF representation of the Spyderisk V1 core model, but has grown into a versioning scheme and associated deployment scripts. The core model is described in `ontology/core.ttl`, and the versioning scheme is described in `ontology/endpoint.ttl`. [ShEx](https://shex.io) shape expressions (see corresponding `.shex` files in `ontology/`) are used to impose constraints, defining the normative shape of valid RDF instance data using these vocabularies.

The deployment scripts are included in `scripts/`, including `ontoconsume`, which injects versioning. The scheme allows us to link versions, and it supports a log of versions (important for provenance and understanding how different versions changed over time). The versioning scheme is broadly similar to that described in [Grandi et al. (2012)](https://doi.org/10.1016/j.jbi.2012.07.005).

Here you will find instructions, scripts and configuration files to run and also deploy an instance of
Ontopublish on a machine you control.

We [welcome contributions to this project](./CONTRIBUTING.md).


