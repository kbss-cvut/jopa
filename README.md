# JOPA - Java OWL Persistence API

JOPA is a Java OWL persistence API aimed at efficient programatic access to OWL2 ontologies in Java. The system is based
on integrity constraints [1] in OWL that JOPA uses to establish the contract between a JOPA-enabled Java application and
an OWL ontology. he system architecture is similar to JPA 2.1, see [2].

### Main Features

* Object-ontological mapping based on integrity constraints,
* Explicit access to inferred knowledge,
* Access to unmapped properties and individual's types
* Transactions
* Separate storage access layer

#### Object-ontological mapping based on integrity constraints

Similarly to ORM, OOM enables to map ontological constructs to constructs of an object-oriented programming language and vice versa.

More specifically, OOM in JOPA maps (we are using JLS terminology):

| Ontology   | OO Language    |
| ---------- | -------------- |
| OWL Class  | Reference type |
| Object property | Reference type member |
| Data property | Primitive type member (+ String, Date) |
| Annotation property | Reference or primitive type member |
| Class assertions | Reference type instance + @Types record |

All this means that individuals belonging to an OWL class can be retrieved as instances of a (Java) class.

#### Explicit access to inferred knowledge

A member annotated with the `@Inferred` annotation represents a field whose values are retrieved using a reasoner. As such,
they can for example contain values of a inverse object property (like in our Jedi example).

There are limitations to this: JOPA requires explicit class assertion to be able to load individual as instance of a class.
And, values of inferred members are read-only. These restrictions have pragmatic reasons - if the knowledge is inferred, it
cannot be directly modified/removed. Therefore, it would make no sense to remove object property value, if it was inferred.

#### Access to unmapped properties and individual's types

OOM is not meant to completely capture the ontological model. It wouldn't even make much sense. One of the main features
of JOPA is its ability to work with knowledge which is not part of the object model. This is done using members annotated
with `@Types` and `@Properties`. `@Types` field contains all OWL classes whose instance the particular individual represented by
an object is. `@Properties` contains values of properties not mapped by object model. This way, the application gets (although limited)
access to for example newly added property values, without the need to adjust object model and recompile.

#### Transactions

JOPA supports object-level transactions. In addition, it makes transactional change visible to the transaction that made them.
This means that when you add an instance of some class during a transaction and then list all instances of that class (during the same
transaction), you'll see the newly added instance as well. This is a feature not usually seen even in large triple stores.

There are some limitations to this approach. Currently, pending changes are not taken into account when doing inference.
Also, the current version of Sesame OntoDriver is not able to include pending changes into results of SPARQL queries.

#### Separate storage access layer

Similarly to JPA and JDBC driver, JOPA sits on top of an OntoDriver instance, which provides access to the underlying storage.
There are two main reasons for such split - firstly, it decouples storage-specific API usage from the more generic OOM core.
Secondly, it enables the application to switch underlying storage with as little as 2-3 lines of configuration code. Nothing else
needs to be modified.

### Not Supported, yet

JOPA currently does not support two important features - inheritance and referential integrity.

Inheritance is currently in planning and development of its support should start soon.

As for referential integrity, this for example means that removing an instance that is referenced by another instance should
not be possible. Such feature is vital for object-oriented application, but not compatible with the open-world nature of ontologies.
Design possibilities and their implications are currently being studied.

Other missing/planned stuff can be found in `TODO.txt`.

## Modules

The whole framework consists of several modules:

* JOPA API - definition of the JOPA API, similar to JPA
* OntoDriver API - API of the storage access layer
* JOPA Implementation - persistence provider implementation
* OntoDriver Sesame - OntoDriver implementation for Sesame-accessed storages
* OntoDriver OWLAPI - OntoDriver implementation for OWLAPI-accessed files
* OWL2Java - generates JOPA entities based on integrity constraints in input ontology (see Example01)

Other modules represent integration tests and various utilities. Jena OntoDriver is planned as future work.
  
## Usage

JOPA examples can be found in a separate repository at [https://github.com/kbss-cvut/jopa-examples](https://github.com/kbss-cvut/jopa-examples).

Note that JOPA requires Java 8.

## Getting JOPA

There are two ways to get JOPA for your project:

* Clone repository/download zip and build it with maven
* Use a Maven dependency from our maven repo at [http://kbss.felk.cvut.cz/m2repo/](http://kbss.felk.cvut.cz/m2repo/)

## More Info

More information about JOPA can be found for example in articles [3], [4] and at [https://kbss.felk.cvut.cz/web/portal/jopa](https://kbss.felk.cvut.cz/web/portal/jopa).

JOPA build status and code metrics can be found at:

* KBSS Jenkins [https://kbss.felk.cvut.cz/jenkins](https://kbss.felk.cvut.cz/jenkins)
* KBSS SonarQube [https://kbss.felk.cvut.cz/sonarqube](https://kbss.felk.cvut.cz/sonarqube)

## References
  
* [1] J. Tao and E. Sirin, J. Bao, D. L. McGuinness, Integrity Constraints in OWL, The Twenty-Fourth AAAI Conference on Artiﬁcial Intelligence, 2010, available online at [http://www.aaai.org/ocs/index.php/AAAI/AAAI10/paper/view/1931/2229](http://www.aaai.org/ocs/index.php/AAAI/AAAI10/paper/view/1931/2229)
* [2] JSR 338 [http://jcp.org/en/jsr/detail?id=338](http://jcp.org/en/jsr/detail?id=338)
* [3] P. Křemen and Z. Kouba: Ontology-Driven Information System Design. IEEE Transactions on Systems, Man, and Cybernetics: Part C, 42(3):334–344, May 2012 [http://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=6011704&tag=1](http://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=6011704&tag=1)
* [4] M. Ledvinka and P. Křemen: JOPA: Accessing Ontologies in an Object-oriented Way. In Proceedings of the 17th International Conference on Enterprise Information Systems. Porto: SciTePress - Science and Technology Publications, 2015, p. 212-222. ISBN 978-989-758-096-0. [http://www.scitepress.org/DigitalLibrary/PublicationsDetail.aspx?ID=p/CdcFwtlFM=&t=1](http://www.scitepress.org/DigitalLibrary/PublicationsDetail.aspx?ID=p/CdcFwtlFM=&t=1)
