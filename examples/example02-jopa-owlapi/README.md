# JOPA Example 02 - JOPA + OWLAPI + Pellet

This example shows usage of JOPA with OWLAPI-based OntoDriver implementation. We are using OWLAPI v4 in the driver.

### Features

Main JOPA features shown in this demo include:

- Inference and inferred fields,
- Operation cascading,
- Participation constraints

#### Inferred Field

The demo features only a single class in the model, but this class contains an important feature of JOPA - support for inferred fields.
Such fields are effectively read-only, so their value changes only based on some other knowledge asserted in the storage. In our case,
the `hasFather` property is an inverse of the `hasChild` property.

The demo uses Pellet for inference, however any other OWLAPI 4-compatible reasoner can be used. Reasoner factory class is specified in `PersistenceFactory`.

#### Cascaded EntityManager operations

When a `Jedi` is persisted, his children are persisted as well. This also works when he is removed from the storage. This is because the `children`
attribute is marked to cascade all operations that happen to the owner instance. Therefore, when a Jedi with children is persisted, the operation is cascaded
to the children instances as well.

#### Participation Constraints

Participation constraints enable the application to place constraints on the number of attribute filler instances. The constraint on `firstName` shows a 
shortened notation which can be used on both singular and plural attributes. For singulars, it means that the field value cannot be null, for plurals it means
that at least one value has to be present in the collection.

The constraint on `lastName` shows a more verbose and powerful syntax, which can be used to set arbitrary participation constraints. If both the shorter and longer 
syntax are used, the longer syntax (an array of `@ParticipationConstraint` values) takes precedence.

Of course, singular fields implicitly express a maximum cardinality of one.


## Persistence Setup

The persistence is set up in `cz.cvut.kbss.jopa.example02.PersistenceFactory`. OWLAPI supports storing ontologies in OWL files with various syntaxes.
We are using an existing OWL ontology, which defines the object properties we want to use for inference. To be sure that the ontology is not interfered with and
thus stays reusable, the application copies the ontology into a separate file, which is then used as storage for the application.


## Running the Demo

To run the demo, `mvn exec:java` can be used. Pellet is available in our maven repository, as is specified in `pom.xml`.