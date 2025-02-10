# Initial modelling considerations

## Preamble

I was working from a UML-like diagram with the following considerations.
1. Arrows with a white triangle at head: these indicate that a child class is member of a parent class
2. Arrows with a black diamond at head: these indicate that a parent entity is composed of a child entity
3. Arrows with a white diamond at head: these indicate that an entity incorporates child entity, but former exists independently of latter.
4. Arrows with a simple 'v' at head: these indicate a reference to the second, but not vice versa.
5. Simple line: these indicate an association, with no specific properties.

Hence there are effectively three core sections to model:
1. Causal entities. These form attributes of other model components (and each other) and they are organised in a class/sub-class hierarchy.
2. Patterns, which are rules matching on (asset) nodes and their positional information. I think the nodes and edges are implicit in the structure of the data, but they depend on edge-level attributes, so I'm not sure how to model that in RDF yet.
3. Assets, which form part of the target system.

## Vocabularies

This document uses the following prefixes (`score` is this V1 core model RDF representation, `my` is hypothetical RDF data, re-using portions of the core model):
```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdfg: <http://www.w3.org/2004/03/trix/rdfg-1/> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix score: <http://ontology.spyderisk.org/ns/core#> .
@prefix my: <http://ontology.spyderisk.org/p/example#> .
```

Not everything can be modelled effectively with RDF and RDF/S properties and classes alone. This vocabulary goes beyond RDF/S by introducing [SKOS](https://www.w3.org/2004/02/skos/). SKOS is useful because:
- RDF/S classes and sub-classes are useful, but sometimes we want a similar hierarchical relationship, without implying class inheritance. Using SKOS for this also enriches our descriptions of concepts, and how they relate to each other.
- SKOS has notions of broader and narrower generalisations of concepts, as well as imposing order on these.
- SKOS provides a rich interface for extending and elaborating concepts which have already declared, like we do in this vocabulary.

What SKOS does not get us is things like constraints, such as on cardinality. For this, one would need to use something like OWL, or one of the newer shape constraints languages like [ShEx](https://shex.io/) or [SHACL](https://www.w3.org/TR/shacl/).

## Shaded items

### Implicit `rdf:type`

Excepting the shaded items relating to asset behaviours, shaded items are 'types' of certain classes, which are concepts elaborated in the domain model. In fact, the first decision I made during modelling was that these should not be explicitly part of the core model. Instead, these should be instantiated by the domain model. We get this for free from using RDF and RDF/S.

The following statement in the `core.ttl` vocabulary is made, declaring a threat:
```turtle
score:Threat rdf:type rdfs:Class ;
    rdfs:subClassOf score:CausalEntity .
```
Class inheritance means that `score:Threat` inherits a `score:Likelihood` property from `score:CausalEntity`. The corresponding instantiation of a threat, can be made in a different RDF graph:
```turtle
my:PersistentThreat rdf:type score:Threat .
```
In the RDF world, this kind of usage is key to incremental re-use and elaboration of concepts. This statement is straightforward, saying that `my:PersistentThreat` has type `score:Threat`. Support for these statements is also implicit to our use of RDF. We don't need to define a 'type' edge for each element of our core model. 

(Note that in RDF graphs, simple `a` is syntactic sugar for an `rdf:type` triple predicate, and is used as it is terser than writing out `rdf:type` all the time.)

### Asset behaviours / threat effects

Asset behaviours are modelled slightly differently to the other shaded items. Following meeting last week, I modelled it as follows:

1. Broadly speaking, there is an asset behaviour triggered by a threat pattern. This is a sub-class of Asset Property. For example, a threat might produce a large amount of traffic targeted at a network interface, and so the asset behaviour might be modelled with the name `my:NetworkInterfaceUnderLoad`.
2. This behaviour then undermines asset trustworthiness. This is an edge between `my:NetworkInterfaceUnderLoad` and an asset trustworthiness attribute, for example `my:CoreVPNReachability`. This edge can then be *attributed* with one or more behaviour *types* proper, for example `my:high_CPU_load`, `my:disk_poor_IO`, `my:interface_flapping`.

![behaviour-attrs-single](https://raw.githubusercontent.com/Spyderisk/ontopublish/main/ontology/attrs-1.svg)

The main argument to model this in this manner is that, while it is possible to model this using sub-classes (adding a 'behaviour type'), this would create an edge per 'behaviour type undermines trustworthiness attribute', which is unwieldy. This approach instead elaborates on the general behaviour, and it is feasible that it allows more granular groupings. For example, `my:NetworkInterfaceUnderLoad` might undermine more than one behaviours, in different ways, like the following:

![behaviour-attrs-multi](https://raw.githubusercontent.com/Spyderisk/ontopublish/main/ontology/attrs-2.svg)

## Patterns

Patterns can modelled structurally using named graphs. Named graphs are collections of RDF statements, which are associated with an identifier. The reasoning behind named graphs' conception is essentially that RDF documents often already have a name associated with them, and it should be something that can be referenced or interrogated. See (here)[https://sven-lieber.org/en/2023/06/26/rdf-named-graphs/] for an introduction to named graphs, and (here)[https://www.w3.org/2009/12/rdf-ws/p613.pdf] for a discussion of their semantics.

The point here is that we're matching on collections of assets, and relationships between them, notwithstanding the *role* of a node. Named graphs allow us to describe the structure of a pattern, and patterns' association with threats and control strategies, while describing the logic layer acting on the structure of the pattern elsewhere.

![pattern-pre](https://raw.githubusercontent.com/Spyderisk/ontopublish/main/ontology/named-graphs.svg)

Threat patterns are related to control strategies and threats, in that both can introduce them. In this diagram, patterns have an RDF identifier/IRI (e.g. `my:my:NFSSharePattern`). Note that in principle, there is no reason why a pattern cannot be worked on by multiple threats or control strategies. It could be a common pattern, or both patterns of assets could be similarly threatened. This is expressed in the diagram where we see that the pattern structure `my:NFSSharePattern` is introduced by all three threats + control strategies.

![pattern-post](https://raw.githubusercontent.com/Spyderisk/ontopublish/main/ontology/named-graphs-replacement.svg)

In this diagram, the identifiers are replaced with the graph they stand for. These graphs serve as the patterns of assets, and relations between them, which will be matched by the pattern-matching implementation. 

In terms of restricting these to instances of assets and relations, since named graphs' identifiers stand in for the named RDF graph, standard techniques should be feasible for defining constraints. (See the "Vocabularies" section above.)

## Causal entities and likelihoods

Similarly, as given, threats, threat causes, and asset properties are considered to be sub-classes of causal entities (things with likelihood). The motivation here appears to be that these are all things which can have a likelihood.

I have modelled this in two ways. First, I have modelled 'causal entity' as part of a SKOS concept scheme. The hierarchy is modelled using `skos:broader` and `skos:broaderTransitive`. Second, I have associated everything defined as a sub-class of 'causal entity' to have a 'likelihood' property. This likelihood is also modelled as a SKOS concept scheme, with order imposed using `skos:OrderedCollection`.

## Next steps

The vocabulary does not currently develop constraints on cardinality, as given. The exact approach to employ here is not clear:
- We may not want to adopt OWL, which is rich, but also complex. Nonetheless, invoking `skos:Concept` does imply one thing: anything we associate with it is also an `owl:Class`. It is feasible that we may want to adopt certain elements of OWL.
- Shape constraints languages like ShEx or SHACL are relatively new, and would likely be suitable here. The key advantage is that they may be less complex than certain concepts from OWL. They let us describe the shape of data and apply constraints.

The way in which we enforce constraints depends on the constraints we need, and how we intend to enforce them, neither of which are especially clear.
