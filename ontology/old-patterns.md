# Patterns

Patterns can be modelled structurally using named graphs. Named graphs are collections of RDF statements, which are associated with an identifier. The reasoning behind named graphs' conception is essentially that RDF documents often already have a name associated with them, and it should be something that can be referenced or interrogated. See (here)[https://sven-lieber.org/en/2023/06/26/rdf-named-graphs/] for an introduction to named graphs, and (here)[https://www.w3.org/2009/12/rdf-ws/p613.pdf] for a discussion of their semantics.

The point here is that we're matching on collections of assets, and relationships between them, notwithstanding the *role* of a node. Named graphs allow us to describe the structure of a pattern, and patterns' association with threats and control strategies, while describing the logic layer acting on the structure of the pattern elsewhere.

![pattern-pre](https://raw.githubusercontent.com/Spyderisk/ontopublish/main/ontology/named-graphs.svg)

Threat patterns are related to control strategies and threats, in that both can introduce them. In this diagram, patterns have an RDF identifier/IRI (e.g. `my:my:NFSSharePattern`). Note that in principle, there is no reason why a pattern cannot be worked on by multiple threats or control strategies. It could be a common pattern, or both patterns of assets could be similarly threatened. This is expressed in the diagram where we see that the pattern structure `my:NFSSharePattern` is introduced by all three threats + control strategies.

![pattern-post](https://raw.githubusercontent.com/Spyderisk/ontopublish/main/ontology/named-graphs-replacement.svg)

In this diagram, the identifiers are replaced with the graph they stand for. These graphs serve as the patterns of assets, and relations between them, which will be matched by the pattern-matching implementation. 

In terms of restricting these to instances of assets and relations, since named graphs' identifiers stand in for the named RDF graph, standard techniques should be feasible for defining constraints over these very graphs. (See the "Vocabularies" section above.)
