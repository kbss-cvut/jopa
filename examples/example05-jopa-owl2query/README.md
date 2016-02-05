# JOPA Example 05 - JOPA + OWL2Query

This example shows how JOPA works with OWL2Query, which provides SPARQL query support for OWLAPI-based ontologies.

### Features

This example shows query support when accessing OWL ontologies with JOPA. In addition, unmapped properties are used.

### OWL2Query

[OWL2Query](https://kbss.felk.cvut.cz/web/portal/owl2query) is a query engine with OWL 2 and SPARQL-DL with negation as failure support. 
It is used by default by the OWLAPI OntoDriver. In this example, [Pellet](https://github.com/Complexible/pellet) is used to evaluate
the queries.

### Unmapped Properties

It is not always possible (and/or desirable) to capture the whole ontology schema with the object model. If it were, there would be
little reason to use ontologies over relational databases (apart from inference, of course). JOPA provides (limited) access
to property values of an individual, which are not mapped by the object model.

These values are stored in a field annotated with @Properties. Currently, it is a map with string keys (representing the property IRI)
and sets of strings as values. This somewhat limits the expressiveness of the values, which are always stored as string literals
in the ontology. If a corresponding property is not found in the TBox, it is assumed to be an object property.

## Persistence Setup

The persistence is set up in `cz.cvut.kbss.jopa.example05.persistence.PersistenceFactory`. We are using an existing OWL ontology, 
which defines the object properties we want to use for inference. To be sure that the ontology is not interfered with and
thus stays reusable, the application copies the ontology into a separate file, which is then used as a storage for the application.

## Running the Demo

To run the demo, `mvn exec:java` can be used. Pellet is available in our maven repository, as is specified in `pom.xml`.
