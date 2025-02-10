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
In the RDF world, this kind of pattern is key to incremental re-use and elaboration of concepts. This statement is straightforward, saying that `my:PersistentThreat` has type `score:Threat`. Support for these statements is also implicit to our use of RDF. We don't need to define a 'type' edge for each element of our core model. 

(Note that in RDF graphs, simple `a` is syntactic sugar for an `rdf:type` triple predicate, and is used as it is terser than writing out `rdf:type` all the time.)

### Asset behaviours / threat effects

Asset behaviours are modelled slightly differently to the other shaded items. Following meeting last week, I modelled it as follows:

1. Broadly speaking, there is an asset behaviour triggered by a threat pattern. This is a sub-class of Asset Property. For example, a threat might produce a large amount of traffic targeted at a network interface, and so the asset behaviour might be modelled with the name `my:NetworkInterfaceUnderLoad`.
2. This behaviour then undermines asset trustworthiness. This is an edge between `my:NetworkInterfaceUnderLoad` and an asset trustworthiness attribute, for example `my:CoreVPNReachability`. This edge can then be *attributed* with one or more behaviour *types* proper, for example `my:high_CPU_load`, `my:disk_poor_IO`, `my:interface_flapping`.

![behaviour-attrs-single](https://raw.githubusercontent.com/Spyderisk/ontopublish/main/ontology/attrs-1.svg)

The main argument to model this in this manner is that, while it is possible to model this using sub-classes (adding a 'behaviour type'), this would create an edge per 'behaviour type undermines trustworthiness attribute', which is unwieldy. This approach instead elaborates on the general behaviour, and it is feasible that it allows more granular groupings. For example, `my:NetworkInterfaceUnderLoad` might undermine more than one behaviours, in different ways, like the following:

![behaviour-attrs-multi](https://raw.githubusercontent.com/Spyderisk/ontopublish/main/ontology/attrs-2.svg)

## Patterns

**(Note that this has been revised, so this portion of the document reflects the first iteration, and will be updated accordingly later.)**

This was the most difficult part of the modelling exercise. It is not clear how best to model this. A pattern has a 'node' and a 'link' component. There are a several ways in which we might approach this:
1. Model a pattern using simple classes and properties. Comprising a parent class with sub-properties source node and link, sub-properties should be classes with additional properties. First, node should have a role type: 'unique', 'redundant' &c. Second, the link should connect a source and a target asset, and similarly there should be a link type: 'mandatory' or 'prohibited'. 
2. Model this at a different level of abstraction. Consider the pattern as made up of a source node, a target node, and the type of the directed edge spanning these. These three sub-properties can be considered as:
   1. an edge from the pattern to the source node's type (e.g. `my:SerialTerminal`)
   2. an edge from the pattern to the target node's type (e.g. `my:DesktopWorkstation`
   3. an edge from the pattern to the spanning edge's type (e.g. `my:RS232`). 

The trick here is to further *attribute* these edges. These would be the role of the source node (e.g. 'unique'), role of the target node, and relationship type of the spanning edge (e.g. 'prohibited'). Although the link type may only appear in the system model, it is useful to define them in the core model as it supports re-use and elaboration downstream.

The former approach is significantly cumbersome. It involves defining a number of classes and sub-classes. In contrast, the second approach associates a pattern with much fewer properties, albeit attributes on edges in RDF can be difficult to reason about (essentially an edge which spans an edge and a node).

Selecting the second approach, this involves a couple of things:
1. Defining a general `score:asset_relation` property for spanning edges between causal entities, which can be elaborated by downstream vocabularies or instances. Defining this is necessary because the `score:match_via` links a pattern to this spanning edge type.
2. Complementing `score:match_via` with `score:match_on_source` and `score:match_on_target`, which associate pattern with a source and target asset node respectively. These can then be attributed by downstream vocabularies or instances using the `score:conditioned_via` and `score:conditioned_on` properties, which associate a link with a `score:SpanningAttribute` and `score:RoleAttribute` respectively.
3. Role attributes are defined as sub-classes of `score:RoleAttribute`, which is also a SKOS concept scheme. Likewise, spanning attributes are defined similarly.

## Causal entities and likelihoods

Similarly, as given, threats, threat causes, and asset properties are considered to be sub-classes of causal entities (things with likelihood). The motivation here appears to be that these are all things which can have a likelihood.

I have modelled this in two ways. First, I have modelled 'causal entity' as part of a SKOS concept scheme. The hierarchy is modelled using `skos:broader` and `skos:broaderTransitive`. Second, I have associated everything defined as a sub-class of 'causal entity' to have a 'likelihood' property. This likelihood is also modelled as a SKOS concept scheme, with order imposed using `skos:OrderedCollection`.

## Next steps

The vocabulary does not currently develop constraints on cardinality, as given. The exact approach to employ here is not clear:
- We may not want to adopt OWL, which is rich, but also complex. Nonetheless, invoking `skos:Concept` does imply one thing: anything we associate with it is also an `owl:Class`. It is feasible that we may want to adopt certain elements of OWL.
- Shape constraints languages like ShEx or SHACL are relatively new, and would likely be suitable here. The key advantage is that they may be less complex than certain concepts from OWL. They let us describe the shape of data and apply constraints.

The way in which we enforce constraints depends on the constraints we need, and how we intend to enforce them, neither of which are especially clear.
