# 'Core' ontology RDF modelling exercise

<img src="https://raw.githubusercontent.com/Spyderisk/ontopublish/main/ontology/semifinal.svg" />

## Preamble

<img align="right" src="https://raw.githubusercontent.com/Spyderisk/ontopublish/main/ontology/pre-transcription.svg" height="600" />

I was working from the UML-like diagram to the right, with the following considerations:
- Arrows with a white triangle at head: these indicate that a child class is member of a parent class
- Arrows with a black diamond at head: these indicate that a parent entity is composed of a child entity
- Arrows with a white diamond at head: these indicate that an entity incorporates child entity, but former exists independently of latter.
- Arrows with a simple 'v' at head: these indicate a reference to the second, but not vice versa.
- Simple line: these indicate an association, with no specific properties.

There are effectively three core sections to model:
1. Causal entities. These form attributes of other model components (and each other) and they are organised in a class/sub-class hierarchy.
2. Threats and threat roles (originally patterns of assets and relations). Contextualise assets' roles within a threat.
3. Structural entities. These comprise assets and relations between which, and form part of the target system.

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
In the RDF world, this kind of usage is key to incremental re-use and elaboration of concepts. This statement is straightforward, saying that `my:PersistentThreat` has type `score:Threat`. Support for these statements is also implicit to our use of RDF. We don't need to define a 'type' edge for each element of our core model. The RDF model shown at the very start of the document consequently omits almost all of the shaded types.

(Note that in RDF graphs, simple `a` is syntactic sugar for an `rdf:type` triple predicate, and is used as it is terser than writing out `rdf:type` all the time.)

### Asset behaviours / threat effects

Asset behaviours are modelled slightly differently to the other shaded items. Following meeting last week, I modelled it as follows.

Broadly speaking, an asset behaviour is caused by a threat. This is a sub-class of Asset Property. For example, a threat might produce a large amount of traffic targeted at a network interface, and so the asset behaviour might be modelled with the name `my:NetworkInterfaceUnderLoad`.

<img src="https://raw.githubusercontent.com/Spyderisk/ontopublish/main/ontology/attrs-singleton.svg" />

This behaviour then undermines trustworthiness of an asset. In the above diagram, there are two asset behaviours, `my:NetworkInterfaceUnderLoad` and `my:NetworkInterfaceRaceCondition`. It can be observed that the network interface under load undermines the reachability of an SSH 'bastion' host, by making the interface flap (the interface is unreliable, and SSH responds poorly to this). The other case involves some kind of race condition around network interfaces, with high CPU load undermining the protection of certain ports by a firewall (for example, a poorly-configured firewall may not come up until after a delay, exposing ports).

In terms of the modelling strategy, note how edges are attributed with behaviour *types*. In the diagram, the attributes are `my:high_CPU_load` and `my:interface_flapping`. Modelling in this manner avoids creating a class per behaviour type, which is unwieldy, with an edge between the general asset behaviour and the behaviour type, and then between the behaviour type and the asset trustworthiness attribute.

## Threat role
    
Threat roles, formerly 'nodes', are primarily a general role (like attacker subnet), which can be additionally narrowed down to specific classes of asset. 

If they were just collections of assets, then there would be an argument to just model an edge between threat and asset, and to attribute that with a possible role. Indeed, modelling it in this way means that roles can be more easily re-used, even if it is relatively uncommon for a role to be meaningful outside of a threat. 

On discussion, 'node' has been renamed to "threat role", as 'node' is vague and rather confusing when discussing what is notwork data. Further, these threat roles only apply to a certain type of structural entity, and rather capture the role within a threat. Part of this discussion also involved dispelling with patterns at this stage. We do not anticipate modelling these at all.

## Causal entities and likelihoods

Similarly, as given, threats, threat causes, and asset properties are considered to be sub-classes of causal entities (things with likelihood). The motivation here appears to be that these are all things which can have a likelihood.

I have modelled this in two ways. First, I have modelled 'causal entity' as part of a SKOS concept scheme. The hierarchy is modelled using `skos:broader` and `skos:broaderTransitive`. Second, I have associated everything defined as a sub-class of 'causal entity' to have a 'likelihood' property. This likelihood is also modelled as a SKOS concept scheme, with order imposed using `skos:OrderedCollection`.

## Next steps

The vocabulary does not currently develop constraints on cardinality, as given. The exact approach to employ here is not clear:
- We may not want to adopt OWL, which is rich, but also complex. Nonetheless, invoking `skos:Concept` does imply one thing: anything we associate with it is also an `owl:Class`. It is feasible that we may want to adopt certain elements of OWL.
- Shape constraints languages like ShEx or SHACL are relatively new, and would likely be suitable here. The key advantage is that they may be less complex than certain concepts from OWL. They let us describe the shape of data and apply constraints.

The way in which we enforce constraints depends on the constraints we need, and how we intend to enforce them, neither of which are especially clear.
