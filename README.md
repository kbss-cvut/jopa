# JOPA - Java OWL Persistence API

[![Build Status](https://kbss.felk.cvut.cz/jenkins/buildStatus/icon?job=jopa-stable)](https://kbss.felk.cvut.cz/jenkins/job/jopa-stable)

JOPA is a Java OWL persistence framework aimed at efficient programmatic access to OWL2 ontologies and RDF graphs in Java. 
The system is based on integrity constraints [1] in OWL that JOPA uses to establish a contract between a JOPA-enabled Java application and
an OWL ontology. Note, however, that for practical purposes of working with triple stores, this OWL integrity constraints-based
contract is not required.

The library architecture and API is similar to JPA (see [2]) so that Java developers find it familiar.

### Main Features

* Object-ontological mapping (OOM) based on integrity constraints
* Explicit access to inferred knowledge
* Access to unmapped properties and individual's types
* Transactions
* Multilingual strings
* Separate storage access layer - Jena, OWLAPI, RDF4J drivers are available

#### Object-ontological Mapping Based on Integrity Constraints

Similarly to object-relational mapping (ORM), OOM enables to map ontological constructs to constructs of an object-oriented programming language and vice versa.

More specifically, OOM in JOPA maps (using the JLS [3] terminology):

| Ontology            | OO Language                                                |
|---------------------|------------------------------------------------------------|
| OWL Class           | Reference type                                             |
| Object property     | Reference type member                                      |
| Data property       | Primitive type member (+ String, Date, MultilingualString) |
| Annotation property | Reference or primitive type member                         |
| Class assertions    | Reference type instance or @Types record                   |

All this means that individuals belonging to an OWL class can be retrieved as instances of a (Java) class.

Note: OOM works also for RDFS ontologies. See the [wiki](https://github.com/kbss-cvut/jopa/wiki/Object-ontological-Mapping) for details.

Here is a simple example of a JOPA entity:

```java
@Namespace(prefix = "skos", namespace="http://www.w3.org/2004/02/skos/core#")
@OWLClass(iri = "skos:Concept")
public class Term {
  @Id
  private URI id;

  @OWLAnnotationProperty(iri = "skos:prefLabel")
  private MultilingualString label;

  @OWLAnnotationProperty(iri = "skos:definition")
  private MultilingualString definition;

  @OWLObjectProperty(iri = "skos:narrower")
  private Set<Term> children;

  @Types
  private Set<String> types;

  @Properties
  private Map<String, Set<String>> properties;
}
```

#### Explicit Access to Inferred Knowledge

A member annotated with the `@Inferred` annotation represents a field whose values are retrieved using a reasoner. As such,
they can, for example, contain values of an inverse object property (like in the [Jedi example](https://github.com/kbss-cvut/jopa-examples/blob/master/example02-jopa-owlapi/src/main/java/cz/cvut/kbss/jopa/example02/Example.java)).

There are limitations to this: JOPA requires explicit class assertion to be able to load individual as instance of a class.
And, inferred values are read-only. These restrictions have pragmatic reasons - if the knowledge is inferred, it
cannot be directly modified/removed, so attempting to remove an inferred value does not have any direct effects.

#### Access to Unmapped Properties and Individual's Types

OOM is not meant to completely capture the ontological model. It would not even make much sense. One of the main features
of JOPA is its ability to work with knowledge which is not part of the object model. This is done using members annotated
with `@Types` and `@Properties`. `@Types` field contains all OWL classes whose instance the particular individual represented by
an object is (except the one mapped by the object's Java class). 
`@Properties` field contains values of properties not mapped by object model. This way, the application gets (although limited)
access to unmapped property values (e.g. values of newly added properties), without the need to adjust the object model and recompile.

#### Transactions

JOPA supports object-level transactions. In addition, it makes transactional change visible to the transaction that made them.
This means that when you add an instance of some class during a transaction and then list all instances of that class (during the same
transaction), you'll see the newly added instance as well.

There are limitations to this approach. Currently, pending changes are not taken into account when doing inference.
Also, the current version of RDF4J OntoDriver is not able to include pending changes into results of SPARQL queries.

#### Separate Storage Access Layer

Similarly to JPA and JDBC driver, JOPA sits on top of an OntoDriver instance, which provides access to the underlying storage.
There are two main reasons for such a split - first, it decouples storage-specific API usage from the more generic OOM core.
Second, it enables the application to switch the underlying storage with as little as 2-3 lines of configuration code. Nothing else
needs to be modified.

Supported storages:

* [Jena](https://jena.apache.org/)
* [OWLAPI](https://github.com/owlcs/owlapi)
* [RDF4J](https://rdf4j.org/)
* [Virtuoso](https://virtuoso.openlinksw.com/)

## Modules

The whole framework consists of several modules:

* _JOPA API_ - definition of the JOPA API, similar to JPA.
* _OntoDriver API_ - API of the storage access layer.
* _JOPA implementation_ - persistence provider implementation.
* _OntoDriver RDF4J_ - OntoDriver implementation for RDF4J-accessed storages.
* _OntoDriver OWLAPI_ - OntoDriver implementation for OWLAPI-accessed files.
* _Ontodriver Jena_ - OntoDriver implementation for Jena-based storages.
* _OntoDriver Virtuoso_ - OntoDriver implementation for Virtuoso database server.
* _OWL2Java_ - generates JOPA entities based on integrity constraints in input ontology (see [Example01](https://github.com/kbss-cvut/jopa-examples/tree/master/example01-jopa-rdf4j-owl2java)).
* _Modelgen_ - [static metamodel generator](https://github.com/kbss-cvut/jopa/wiki/Static-Metamodel).
* _JOPA Maven plugin_ - Maven plugin for object model (using OWL2Java) and static metamodel (using Modelgen) generation.

Other modules represent integration tests and various utilities.

## Documentation

Check out the [Wiki](https://github.com/kbss-cvut/jopa/wiki) for general information about JOPA, explanation of its features and their usage.
The content is being gradually created and updated.

Javadoc of the latest published version is available at [https://kbss.felk.cvut.cz/jenkins/job/jopa-stable/javadoc/index.html?overview-summary.html](https://kbss.felk.cvut.cz/jenkins/job/jopa-stable/javadoc/index.html?overview-summary.html).

For practical examples of JOPA features, see the [JOPA examples repository](https://github.com/kbss-cvut/jopa-examples).
  
## Usage

JOPA examples can be found in a separate repository at [https://github.com/kbss-cvut/jopa-examples](https://github.com/kbss-cvut/jopa-examples).

A real-world, up-to-date project using JOPA is [TermIt](https://github.com/kbss-cvut/termit) - a SKOS-compatible vocabulary manager.

Note that JOPA requires **Java 17** or later.

## Getting JOPA

There are two ways of getting JOPA for a project:

* Clone repository/download zip and build it with Maven,
* Use a Maven/Gradle dependency from the [Maven central repository](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22cz.cvut.kbss.jopa%22).

Basically, the _jopa-impl_ module and one of the OntoDriver implementations is all that is needed:

```xml
<dependencies>
    <dependency>
        <groupId>cz.cvut.kbss.jopa</groupId>
        <artifactId>jopa-impl</artifactId>
    </dependency>
    <dependency>
        <groupId>cz.cvut.kbss.jopa</groupId>
        <artifactId>ontodriver-rdf4j</artifactId>
        <!-- OR <artifactId>ontodriver-jena</artifactId> -->
        <!-- OR <artifactId>ontodriver-owlapi</artifactId> -->
        <!-- OR <artifactId>ontodriver-virtuoso</artifactId> -->
    </dependency>
</dependencies>
```

## More Info

More information about JOPA can be found, for example, in articles [4], [5], [6] and on the GitHub [Wiki](https://github.com/kbss-cvut/jopa/wiki).

JOPA build status and code metrics can be found at:

* KBSS Jenkins [https://kbss.felk.cvut.cz/jenkins](https://kbss.felk.cvut.cz/jenkins),
* KBSS SonarQube [https://kbss.felk.cvut.cz/sonarqube](https://kbss.felk.cvut.cz/sonarqube).

### Performance

A performance comparison of JOPA and other object-triple mapping libraries can be found at [https://kbss.felk.cvut.cz/web/otm-benchmark](https://kbss.felk.cvut.cz/web/otm-benchmark).

A comprehensive comparison - feature and performance - of object-triple mapping libraries is presented in [7].

## Related

Some related libraries:

* [JB4JSON-LD](https://github.com/kbss-cvut/jb4jsonld) - Serialization and deserialization of POJOs into JSON-LD. Uses JOPA mapping annotations.
* [JOPA-Spring-transaction](https://github.com/ledsoft/jopa-spring-transaction) - Declarative Spring transactions (using the `@Transactional` annotation) with JOPA.
* [Reporting Tool](https://github.com/kbss-cvut/reporting-tool) - Real-life use case of JOPA. _No longer actively maintained_.
* [TermIt](https://github.com/kbss-cvut/termit) - A more complex and up-to-date use case of JOPA.

## History

Notable changes:

* **2.3.0** - Add Virtuoso driver.
* **2.0.0** - Major rewrite of change tracking and lazy loading, remove internal API from the `jopa-api` module etc.
* **1.0.0** - Support for static metamodel generation and mapping multiple inheritance via Java interfaces.
* **0.20.0** - Allow editing inferred attributes (See the [wiki](https://github.com/kbss-cvut/jopa/wiki) for more details). Support `IN`, `NOT LIKE`, `<>` operators in SOQL.
* **0.19.0** - Add RDF4J driver (renaming of Sesame driver, which has been deprecated and will be removed in the future).
* **0.17.0** - Support for SPARQL-based entity attributes and Criteria API. See the [wiki](https://github.com/kbss-cvut/jopa/wiki) for more details.

See [CHANGELOG.md](./CHANGELOG.md) for detailed change history.

## References
  
* [1] J. Tao and E. Sirin, J. Bao, D. L. McGuinness, Integrity Constraints in OWL, The Twenty-Fourth AAAI Conference on Artiﬁcial Intelligence, 2010, available online at [https://ojs.aaai.org/index.php/AAAI/article/view/7525/7386](https://ojs.aaai.org/index.php/AAAI/article/view/7525/7386)
* [2] JSR 338 [http://jcp.org/en/jsr/detail?id=338](http://jcp.org/en/jsr/detail?id=338)
* [3] The Java Language Specification, Java SE 8 Edition [https://docs.oracle.com/javase/specs/jls/se8/html/index.html](https://docs.oracle.com/javase/specs/jls/se8/html/index.html)
* [4] P. Křemen and Z. Kouba: Ontology-Driven Information System Design. IEEE Transactions on Systems, Man, and Cybernetics: Part C, 42(3):334–344, May 2012 [https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=6011704](https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=6011704)
* [5] M. Ledvinka and P. Křemen: JOPA: Accessing Ontologies in an Object-oriented Way. In Proceedings of the 17th International Conference on Enterprise Information Systems. Porto: SciTePress - Science and Technology Publications, 2015, p. 212-222. ISBN 978-989-758-096-0. [http://www.scitepress.org/DigitalLibrary/PublicationsDetail.aspx?ID=p/CdcFwtlFM=&t=1](http://www.scitepress.org/DigitalLibrary/PublicationsDetail.aspx?ID=p/CdcFwtlFM=&t=1)
* [6] M. Ledvinka and P. Křemen and B. Kostov: JOPA: Efficient Ontology-based Information System Design In: The Semantic Web: ESWC 2016 Satellite Events. Cham: Springer International Publishing AG, 2016. pp. 156-160. 9989. ISSN 0302-9743. ISBN 978-3-319-47601-8. [ESWC 2016 Demo](http://2016.eswc-conferences.org/sites/default/files/papers/Accepted%20Posters%20and%20Demos/ESWC2016_DEMO_JOPA.pdf)
* [7] M. Ledvinka and P. Křemen: A comparison of object-triple mapping libraries Semantic Web, 2019, doi: [10.3233/SW-190345](http://dx.doi.org/10.3233/SW-190345)

## License

LGPLv3
