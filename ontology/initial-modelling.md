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
2. Construction patterns, which are rules matching on (asset) nodes and their positional information. I think the nodes and edges are implicit in the structure of the data, but they depend on edge-level attributes, so I'm not sure how to model that in RDF yet.
3. Assets, which form part of the target system.

Define the following prefixes (`score` is this V1 core model):
```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
@prefix score: <http://ontology.spyderisk.org/ns/core#> .
```

## Semantics

This vocabulary goes beyond RDF/S by introducing [SKOS](https://www.w3.org/2004/02/skos/). SKOS is useful because it enriches our descriptions of concepts, and how they relate to each other. Not everything can be modelled effectively with RDF and RDF/S properties and classes alone. It is also relatively lightweight: the semantics imply relatively little about what we associate with `skos:Concept`.

What SKOS does not get us is things like constraints, such as on cardinality. For this, one would need to use something like OWL, or one of the newer shape constraints languages like [ShEx](https://shex.io/) or [SHACL](https://www.w3.org/TR/shacl/).

## Patterns

This was the most difficult part of the modelling exercise. It is not clear how best to model this. A pattern has a 'node' and a 'link' component. There are a several ways in which we might approach this:
1. Model a pattern using simple classes and properties. Comprising a parent class with sub-properties source node and link, sub-properties should be classes with additional properties. First, node should have a role type: 'unique', 'redundant' &c. Second, the link should connect a source and a target asset, and similarly there should be a link type: 'mandatory' or 'prohibited'. 
2. Model this at a different level of abstraction. Consider the pattern as made up of a source node, a target node, and the type of the directed edge spanning these. These three sub-properties can be considered as:
   1. an edge from the pattern to the source node's type (e.g. `:SerialTerminal`)
   2. an edge from the pattern to the target node's type (e.g. `:DesktopWorkstation`
   3. an edge from the pattern to the spanning edge's type (e.g. `:RS232`). 

The trick here is to further *attribute* these edges. These would be the role of the source node (e.g. 'unique'), role of the target node, and relationship type of the spanning edge (e.g. 'prohibited'). Although the link type may only appear in the system model, it is useful to define them in the core model as it supports re-use and elaboration downstream.

The former approach is significantly cumbersome. It involves defining a number of classes and sub-classes. In contrast, the second approach associates a pattern with much fewer properties, albeit attributes on edges in RDF can be difficult to reason about (essentially an edge which spans an edge and a node).

Selecting the second approach, this involves a couple of things:
1. Defining a general `score:asset_relation` property for spanning edges between causal entities, which can be elaborated by downstream vocabularies or instances. Defining this is necessary because the `score:match_via` links a pattern to this spanning edge type.
2. Complementing `score:match_via` with `score:match_on_source` and `score:match_on_target`, which associate pattern with a source and target asset node respectively. These can then be attributed by downstream vocabularies or instances using the `score:conditioned_via` and `score:conditioned_on` properties, which associate a link with a `score:SpanningAttribute` and `score:RoleAttribute` respectively.
3. Role attributes are defined as sub-classes of `score:RoleAttribute`, which is also a SKOS concept scheme. Likewise, spanning attributes are defined similarly.

## Asset (control) properties

As given, the distinction between asset control properties and asset properties is unclear. Indeed, the causal model is defined by the set (A, T, B, W, C, G): assets, threats, behaviours, trustworthiness controls and control strategies. As such, I have treated asset control properties and asset properties as one. Hence, both assets and control strategies may have an asset property.

## Causal entities and likelihoods

Similarly, as given, threats, threat causes, and asset properties are considered to be sub-classes of causal entities (things with likelihood). The motivation here appears to be that these are all things which can have a likelihood.

I have modelled this in two ways. First, I have modelled 'causal entity' as part of a SKOS concept scheme. The hierarchy is modelled using `skos:broader` and `skos:broaderTransitive`. Second, I have associated everything defined as a sub-class of 'causal entity' to have a 'likelihood' property. This likelihood is also modelled as a SKOS concept scheme, with order imposed using `skos:OrderedCollection`.

## Asset behaviours and trustworthiness

The 'type' of an asset behaviour undermines the 'trustworthiness' of an external cause. We also see that asset property is a super-class of asset property, so we can infer that if an asset property is also associated with a *measure* of trust. 
1. For asset behaviour (a.k.a. threat effect), we have an instantiation or sub-class of which, called 'behaviour type'.
2. For asset trustworthiness attribute (a.k.a. external cause) we have an instantiation or sub-class of which, called 'trustworthiness attribute'.
3. There is a directed edge of cardinality 1, 'undermines'. So asset behaviours undermine asset trustworthiness attributes.

## Shaded items

Excepting the shaded items relating to asset behaviours, shaded items are 'types' of certain classes, which are concepts elaborated in the domain model. In fact, the first decision I made during modelling was that these should not be explicitly part of the core model. Instead, these should be instantiated by the domain model. We get this for free from using RDF and RDF/S.

## Next steps

The vocabulary does not currently develop constraints on cardinality, as given. The exact approach to employ here is not clear:
- We may not want to adopt OWL, which is rich, but also complex. Nonetheless, invoking `skos:Concept` does imply one thing: anything we associate with it is also an `owl:Class`. It is feasible that we may want to adopt certain elements of OWL.
- Shape constraints languages like ShEx or SHACL are relatively new, and would likely be suitable here. The key advantage is that they may be less complex than certain concepts from OWL. They let us describe the shape of data and apply constraints.

The way in which we enforce constraints depends on the constraints we need, and how we intend to enforce them, neither of which are especially clear.
