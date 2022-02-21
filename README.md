# JOPA - Java OWL Persistence API

[![Build Status](https://kbss.felk.cvut.cz/jenkins/buildStatus/icon?job=jopa-stable)](https://kbss.felk.cvut.cz/jenkins/job/jopa-stable)

JOPA is a Java OWL persistence framework aimed at efficient programmatic access to OWL2 ontologies and RDF graphs in Java. The system is based
on integrity constraints [1] in OWL that JOPA uses to establish the contract between a JOPA-enabled Java application and
an OWL ontology. The system architecture and API is similar to JPA 2.1, see [2].

Notable changes:

* **0.17.0** - Support for SPARQL-based entity attributes and Criteria API. See the [wiki](https://github.com/kbss-cvut/jopa/wiki) for more details.
* **0.15.0** - Support for multilingual String attributes. See the [wiki](https://github.com/kbss-cvut/jopa/wiki/Multilingual-String-Attributes) for more info
* **0.14.0** - Support for the Semantic Object Query Language (SOQL). See the [wiki](https://github.com/kbss-cvut/jopa/wiki/Semantic-Object-Query-Language) for more info
* **0.13.0** - Support for RDF simple literals (language-less strings), access to lexical form of literals, updated named graph handling
* **0.12.0** - Support for `EntityManager.getReference`
* **0.11.0** - Support for Java 8 Streams in Query API and RDF4J repository configuration file
* **0.10.0** - Jena OntoDriver implementation
* **0.9.0** - Single class inheritance support
* **0.8.0** - Mapped superclass support

### Main Features

* Object-ontological mapping (OOM) based on integrity constraints,
* Explicit access to inferred knowledge,
* Access to unmapped properties and individual's types,
* Transactions,
* Separate storage access layer - Jena, OWLAPI, RDF4J (Sesame) drivers are available.

#### Object-ontological mapping based on integrity constraints

Similarly to ORM, OOM enables to map ontological constructs to constructs of an object-oriented programming language and vice versa.

More specifically, OOM in JOPA maps (using the JLS [3] terminology):

| Ontology   | OO Language    |
| ---------- | -------------- |
| OWL Class  | Reference type |
| Object property | Reference type member |
| Data property | Primitive type member (+ String, Date) |
| Annotation property | Reference or primitive type member |
| Class assertions | Reference type instance or @Types record |

All this means that individuals belonging to an OWL class can be retrieved as instances of a (Java) class.

#### Inheritance

* Support for mapped superclasses was added in version __0.8.0__.
* Support for single inheritance was added in version __0.9.0__.

#### Explicit access to inferred knowledge

A member annotated with the `@Inferred` annotation represents a field whose values are retrieved using a reasoner. As such,
they can for example contain values of a inverse object property (like in [our Jedi example](https://github.com/kbss-cvut/jopa-examples/blob/master/example02-jopa-owlapi/src/main/java/cz/cvut/kbss/jopa/example02/Example.java)).

There are limitations to this: JOPA requires explicit class assertion to be able to load individual as instance of a class.
And, values of inferred members are read-only. These restrictions have pragmatic reasons - if the knowledge is inferred, it
cannot be directly modified/removed. Therefore, it would make no sense to remove object property value, if it was inferred.

#### Access to unmapped properties and individual's types

OOM is not meant to completely capture the ontological model. It would not even make much sense. One of the main features
of JOPA is its ability to work with knowledge which is not part of the object model. This is done using members annotated
with `@Types` and `@Properties`. `@Types` field contains all OWL classes whose instance the particular individual represented by
an object is. `@Properties` field contains values of properties not mapped by object model. This way, the application gets (although limited)
access to unmapped property values (e.g. values of newly added properties), without the need to adjust the object model and recompile.

#### Transactions

JOPA supports object-level transactions. In addition, it makes transactional change visible to the transaction that made them.
This means that when you add an instance of some class during a transaction and then list all instances of that class (during the same
transaction), you'll see the newly added instance as well. This is a feature not usually seen even in large triple stores.

There are some limitations to this approach. Currently, pending changes are not taken into account when doing inference.
Also, the current version of Sesame OntoDriver is not able to include pending changes into results of SPARQL queries.

#### Separate storage access layer

Similarly to JPA and JDBC driver, JOPA sits on top of an OntoDriver instance, which provides access to the underlying storage.
There are two main reasons for such split - first, it decouples storage-specific API usage from the more generic OOM core.
Second, it enables the application to switch the underlying storage with as little as 2-3 lines of configuration code. Nothing else
needs to be modified.

Supported storages:

* Jena (since **0.10.0**)
* OWLAPI
* Sesame/RDF4J

### Not Supported, yet

JOPA currently does not support two important features - multiple inheritance and referential integrity.

Single inheritance (and mapped superclasses) have been supported since version 0.9.0 (0.8.0 resp.), multiple inheritance is currently in planning.

As for referential integrity, this for example means that removing an instance that is referenced by another instance should
not be possible. Such feature is vital for object-oriented application, but not compatible with the open-world nature of ontologies.
Design possibilities and their implications are currently being studied.

Other missing/planned stuff can be found in [TODO.md](TODO.md) and in the GitHub issue tracker.

## Modules

The whole framework consists of several modules:

* _JOPA API_ - definition of the JOPA API, similar to JPA,
* _OntoDriver API_ - API of the storage access layer,
* _JOPA Implementation_ - persistence provider implementation,
* _OntoDriver Sesame_ - OntoDriver implementation for RDF4J (Sesame)-accessed storages,
* _OntoDriver OWLAPI_ - OntoDriver implementation for OWLAPI-accessed files,
* _Ontodriver Jena_ - OntoDriver implementation for Jena-based storages,
* _OWL2Java_ - generates JOPA entities based on integrity constraints in input ontology (see [Example01](https://github.com/kbss-cvut/jopa-examples/tree/master/example01-jopa-sesame-owl2java)),
* _JOPA Maven plugin_ - Maven plugin for object model generation (using OWL2Java).

Other modules represent integration tests and various utilities.

## Documentation

Check out the [Wiki](https://github.com/kbss-cvut/jopa/wiki) for general information about JOPA, explanation of its features and their usage.
We will be gradually building up its content.

Javadoc of the latest stable version is available at [https://kbss.felk.cvut.cz/jenkins/job/jopa-stable/javadoc/index.html?overview-summary.html](https://kbss.felk.cvut.cz/jenkins/job/jopa-stable/javadoc/index.html?overview-summary.html).

For more practical examples of JOPA features, see the JOPA examples repository at [https://github.com/kbss-cvut/jopa-examples](https://github.com/kbss-cvut/jopa-examples).
  
## Usage

JOPA examples can be found in a separate repository at [https://github.com/kbss-cvut/jopa-examples](https://github.com/kbss-cvut/jopa-examples).

A simplistic demo of JOPA ([https://github.com/kbss-cvut/jopa-examples/tree/master/eswc2016](https://github.com/kbss-cvut/jopa-examples/tree/master/eswc2016)) is running at [http://onto.fel.cvut.cz/eswc2016](http://onto.fel.cvut.cz/eswc2016).

A more mature project using JOPA as its persistence provider can be found at [https://github.com/kbss-cvut/reporting-tool](https://github.com/kbss-cvut/reporting-tool).
It is a safety occurrence reporting tool developed for the aviation industry as part of the INBAS project ([https://www.inbas.cz](https://www.inbas.cz)).
A live demo of it is running at [https://www.inbas.cz/reporting-tool-public](https://www.inbas.cz/reporting-tool-public).

JOPA is also used in the [BINBAS project](https://www.inbas.cz/web/binbas) [6] and, more recently, in [TermIt](https://github.com/kbss-cvut/termit) - a SKOS vocabulary manager.

Note that JOPA requires Java 8 or later.

## Getting JOPA

There are two ways of getting JOPA for your project:

* Clone repository/download zip and build it with Maven,
* Use a Maven dependency from the [Maven central repository](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22cz.cvut.kbss.jopa%22).

Basically, the _jopa-impl_ module and one of the OntoDriver implementations is all that is needed:

```xml
<dependencies>
    <dependency>
        <groupId>cz.cvut.kbss.jopa</groupId>
        <artifactId>jopa-impl</artifactId>
    </dependency>
    <dependency>
        <groupId>cz.cvut.kbss.jopa</groupId>
        <artifactId>ontodriver-jena</artifactId>
        <!-- OR <artifactId>ontodriver-owlapi</artifactId> -->
        <!-- OR <artifactId>ontodriver-sesame</artifactId> -->
    </dependency>
</dependencies>
```

## More Info

More information about JOPA can be found for example in articles [4], [5] and at [https://kbss.felk.cvut.cz/web/kbss/jopa](https://kbss.felk.cvut.cz/web/kbss/jopa).

JOPA build status and code metrics can be found at:

* KBSS Jenkins [https://kbss.felk.cvut.cz/jenkins](https://kbss.felk.cvut.cz/jenkins),
* KBSS SonarQube [https://kbss.felk.cvut.cz/sonarqube](https://kbss.felk.cvut.cz/sonarqube).

### Performance

A performance comparison of JOPA and other object-triple mapping libraries can be found at [https://kbss.felk.cvut.cz/web/kbss/otm-benchmark](https://kbss.felk.cvut.cz/web/kbss/otm-benchmark).

A comprehensive comparison - feature and performance - of object-triple mapping libraries is presented in [8].

## Related

Some related libraries:

* [JB4JSON-LD](https://github.com/kbss-cvut/jb4jsonld) - Serialization and deserialization of POJOs into JSON-LD.
* [JOPA-Spring-transaction](https://github.com/ledsoft/jopa-spring-transaction) - Declarative Spring transactions (using the `@Transactional` annotation) with JOPA.
* [Reporting Tool](https://github.com/kbss-cvut/reporting-tool) - Real-life use case of JOPA.
* [TermIt](https://github.com/kbss-cvut/termit) - A more complex and up-to-date use case of JOPA.

## References
  
* [1] J. Tao and E. Sirin, J. Bao, D. L. McGuinness, Integrity Constraints in OWL, The Twenty-Fourth AAAI Conference on Artiﬁcial Intelligence, 2010, available online at [http://www.aaai.org/ocs/index.php/AAAI/AAAI10/paper/view/1931/2229](http://www.aaai.org/ocs/index.php/AAAI/AAAI10/paper/view/1931/2229)
* [2] JSR 338 [http://jcp.org/en/jsr/detail?id=338](http://jcp.org/en/jsr/detail?id=338)
* [3] The Java Language Specification, Java SE 8 Edition [https://docs.oracle.com/javase/specs/jls/se8/html/index.html](https://docs.oracle.com/javase/specs/jls/se8/html/index.html)
* [4] P. Křemen and Z. Kouba: Ontology-Driven Information System Design. IEEE Transactions on Systems, Man, and Cybernetics: Part C, 42(3):334–344, May 2012 [https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=6011704](https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=6011704)
* [5] M. Ledvinka and P. Křemen: JOPA: Accessing Ontologies in an Object-oriented Way. In Proceedings of the 17th International Conference on Enterprise Information Systems. Porto: SciTePress - Science and Technology Publications, 2015, p. 212-222. ISBN 978-989-758-096-0. [http://www.scitepress.org/DigitalLibrary/PublicationsDetail.aspx?ID=p/CdcFwtlFM=&t=1](http://www.scitepress.org/DigitalLibrary/PublicationsDetail.aspx?ID=p/CdcFwtlFM=&t=1)
* [6] Ledvinka, M.; Křemen, P.; Kostov, B. JOPA: Efficient Ontology-based Information System Design In: The Semantic Web: ESWC 2016 Satellite Events. Cham: Springer International Publishing AG, 2016. pp. 156-160. 9989. ISSN 0302-9743. ISBN 978-3-319-47601-8. [ESWC 2016 Demo](http://2016.eswc-conferences.org/sites/default/files/papers/Accepted%20Posters%20and%20Demos/ESWC2016_DEMO_JOPA.pdf)
* [7] Ledvinka, M.; Křemen, P.; Kostov, B.; Blaško, M. SISel: Aviation Safety Powered by Semantic Technologies In: Data a znalosti 2017. Plzeň: Západočeská univerzita v Plzni, 2017. pp. 77-82. ISBN 978-80-261-0720-0. [https://daz2017.kiv.zcu.cz/data/DaZ2017-Sbornik-final.pdf](https://daz2017.kiv.zcu.cz/data/DaZ2017-Sbornik-final.pdf)
* [8] M. Ledvinka and  P. Křemen: A comparison of object-triple mapping libraries Semantic Web, 2019, doi: [10.3233/SW-190345](http://dx.doi.org/10.3233/SW-190345)

## License

LGPLv3
