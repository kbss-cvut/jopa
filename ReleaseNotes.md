# JOPA - Release Notes


## 0.14.7 - August 4, 2020
- Dependency updates: Jena 3.16.0, OWL API 5.1.16, RDF4J 3.3.0.
- Project configuration simplification.
- Minor fix of formatting `java.util.Date` query parameter values.

## 0.14.6 - June 26, 2020
- Add missing support for mapping `float` attributes.

## 0.14.5 - June 18, 2020
- [JOPA] Extend XSD constants.
- Dependency update - RDF4J 3.2.2.

## 0.14.4 - May 27, 2020
- [JOPA] Fix handling of objects with overridden `equals`/`hashCode` in `PendingReferenceRegistry`.

## 0.14.3 - May 20, 2020
- [OWLAPI OntoDriver, OWLAPI Utils] - Refactor common code in OWL API OntoDriver and OWL API Utils module. [Task #68]
- [OWL2Java] Support mapping **to** remote URLs in the IRI mapping file.
- [JOPA] Support `CollectionAttribute`s. [Task #56]
- [OWL2Java] Sort imported ontology IRIs to ensure stable ordering of generated vocabulary constants.
- Dependency updates - RDF4J 3.2.1, Jena 3.15.0, SLF4J 1.7.30.

## 0.14.2 - April 28, 2020
- [OWL2Java] Do not generate id, label, comment, types and properties fields in entities which already inherit them from parent. (Bug #66, thanks to @cristianolongo for reporting and providing a PR with fix)
- [OWL2Java] Ensure ontology IRI is output into generated vocabularies only once even in case of multiple imports.
- [JOPA] Add constants for all DC Terms properties and classes.
- Dependency updates - RDF4J 3.1.4, OWLAPI 5.1.14.

## 0.14.1 - March 22, 2020
- [JOPA] Support using Java 8 Date/Time API as query parameters.
- [OWL2Java] Log reasons why an ontology could not be imported for transformation.
- Dependency updates - RDF4J 3.1.2.

## 0.14.0 - March 2, 2020
- [JOPA] Implement basic support for the Semantic Object Query Language (SOQL), an object model-based query language. (Feature #19)
- [JOPA] Support annotation properties of type `Object`, which allow to accommodate both literals and URIs.
- Dependency updates - RDF4J 3.1.1.

## 0.13.5 - February 19, 2020
- [JOPA] Fix an issue with loading entity classes in Spring Boot projects, where classes are in a non-standard location in the JAR (Bug #62).

## 0.13.4 - February 12, 2020
- [JOPA] Support using entities as query parameters (Feature #61).
- [JOPA] Run IC validation on lazy loaded fetched attributes also in case no value is loaded.
- [OWLAPI OntoDriver] Search also in imported ontologies when loading data.
- Dependency updates - RDF4J 3.1.0, OWLAPI 5.1.13, Jena 3.14.0, AspectJ 1.9.5. 

## 0.13.3 - November 8, 2019
- Upgraded to RDF4J 3.0.2 and OWLAPI 5.1.12.
- Fixed the issue with incorrect treatment of default context specification for attribute of an entity in a different context (Bug #58).

## 0.13.2 - October 21, 2019
- Upgraded to RDF4J 3.0.1.
- Fixed missing support for lexical form and simple literal plural attributes.
- Automatically generate `toString` method in OWL2Java model transformation.

## 0.13.1 - August 8, 2019
- Fixed incorrect context handling in `ObjectPropertyCollectionDescriptor`.

## 0.13.0 - July 15, 2019
- Added support for read-only literal lexical form access. This allows to load the lexical form of literals regardless of their type (Task #50).
- Added support for RDF *simple literals*, i.e. literals of type `xsd:string`. Simple literals allow to use strings without language tags (Task #53).
- Fixed handling of named graphs. Store property assertions in the subject's graph by default (Task #54).
- Fixed incorrect handling of repository credentials in the RDF4J driver.
- Added support for recursive entity descriptors.
- Updated drivers to always use language specified by assertions and removed global language setting from the drivers.
- Upgraded to OWLAPI 5.1.11, Jena 3.12.0 and RDF4J 2.5.3.

## 0.12.2 - May 28, 2019
- Do not generate error message when directory structure for OWL2Java already exists.
- Fixed issue with parent entity listeners/lifecycle callbacks not being called (Bug #49).
- Fixed problems with using Spring Boot Devtools with JOPA (classloader issues, Task #26).
- Added [SKOS](http://www.w3.org/TR/skos-reference) vocabulary constants.
- Upgraded to AspectJ 1.9.4, Jena 3.11.0 and RDF4J 2.5.2.

## 0.12.1 - March 26, 2019
- Generate ASCII-only identifier names (OWL2Java).
- Fixed issues with caching instances referencing partially loaded getReference results.
- Upgraded to Jena 3.10.0, OWLAPI 5.1.10 and RDF4J 2.5.0.

## 0.12.0 - February 27, 2019
- Implemented support for `EntityManager.getReference` (Feature #46).
- Prevent adding `null` into plural attribute collection when value cannot be loaded (Fix).
- Upgraded to RDF4J 2.4.5.

## 0.11.0 - January 15, 2019
- Added support for specifying repository config when creating embedded in-memory and native RDF4J repositories (Feature #41).
- Added supports for Java 8 Streams in the Query API (Feature #45).
- Fixed issues where aspects were not invoked for fields inherited from mapped superclasses.
- Fixed possible infinite loop caused by entity lifecycle callbacks/listeners modifying the entity itself.
- Upgraded to RDF4J 2.4.2.
- Removed several deprecated parts of the API.

## 0.10.8 - December 12, 2018
- Support for plural annotation properties (Feature #38).
- Fixed incorrect rdf:Property constant.
- Added definitions of a subset of the Dublin Core vocabulary.
- Fixed issues with support of polymorphism in entity attributes.

## 0.10.7 - November 1, 2018
- Added support for Java 8 Date/Time API (Feature #36).
- Fixed issue with caching instances of classes overriding `equals`/`hashCode`.
- Upgraded to AspectJ 1.9.2, Jena 3.9.0, OWL API 5.1.7 and RDF4J 2.4.1.

## 0.10.6 - September 25, 2018
- Support unbound variables in `VariableResult` in `SparqlResultMapping`.
- Fixed incorrect handling of language on attribute loading.
- More consistent handling of open/close persistence context.

## 0.10.5 - August 13, 2018
- Removed several unused files, libraries.
- Improved collection cloning.
- Fixed incorrect field cardinality specification in OWL2Java.
- Upgraded to OWLAPI 5.1.6.
- Added `aop.xml` so that [load time weaving](https://github.com/kbss-cvut/jopa/wiki/Maven-setup#load-time-weaving) can be used with JOPA.

## 0.10.4 - July 20, 2018
- Fixed a critical NPX issue caused by inherited indirect collections not being removed when an entity was detached from 
the persistence context.

## 0.10.3 - July 19, 2018
- Reduced memory footprint.
- Configurable generation of Javadoc from rdfs:comment annotations in OWL2Java (Enhancement #35).
- Entity classes generated by OWL2Java implement `Serializable`.
- Introduced dedicated classes with constants from RDF, RDFS and OWL namespaces into the API.
- Deprecated `CommonVocabulary`, use `RDF`, `RDFS`, `OWL` and `XSD` classes instead.

## 0.10.2 - June 18, 2018
- Fixed bug SPARQL query parser which prevent the use of property paths together with variables.
- Added support for setting the in-memory storage in Jena and RDF4J driver (Enhancement #32).
- Working on Github Wiki.

## 0.10.1 - May 20, 2018
- Fixed bug where old object property assertion was not removed on merge (Bug #33).
- Allow to reload file-based storage in OntoDriver (Enhancement #33).
    - Use the `ReloadableDataSource`, which can be unwrapped from an `EntityManagerFactory`.
- Fixes in the Jena driver. Support for consistency checks.
- Do not mark transaction for rollback on `NoResultException` and `NoUniqueResultException`. 
    This improves compatibility with Spring declarative transaction support (`jopa-spring-transaction`).
- Library updates (AspectJ 1.9.1, RDF4J 2.3.1).
- Support for anonymous ontologies in OWLAPI driver.

## 0.10.0 - April 11, 2018
- Jena OntoDriver implementation (Enhancement #18).
    - In-memory, File and TDB storage are supported.

## 0.9.16 - March 29, 2018
- Fixed bug in cloning instances with circular dependencies (using collections).
- Updated the compiler and AspectJ plugins.

## 0.9.15 - February 21, 2018
- Fixed dependency conflict in JOPA Maven plugin (Bug #29).

## 0.9.14 - February 20, 2018
- Fixed incorrect implementation of `contains` in Sesame driver.
- Minor memory consumption optimizations.
- Fixed volatile order of generated vocabulary elements (Issue #28).

## 0.9.13 - January 14, 2018
- Fixed issue with OWL2Java silently ignoring missing imports (Bug #22).
- Upgraded to OWLAPI 5.1.3. Upgraded to OWL2Query 0.5.0.
- Unified OWLAPI version in all project submodules.
- Switched to Openllet in tests (it supports OWLAPI 5).

## 0.9.12 - December 4, 2017
- Rewrote em.refresh to correspond to JPA behavior (Issue #17).
- Upgraded to the latest RDF4J (2.2.4).
- Generate class names compliant with Java naming conventions (OWL2Java).
- Added support for untyped query parameters to Query API (necessary for `LIMIT` and `OFFSET` support).
- Also added support for `Query.setFirstResult`.

## 0.9.11 - November 14, 2017
- Finished implementation of support for result set mapping - EntityResult (Feature #7).
- Fixed issue with PostLoad lifecycle callbacks and listeners not being called for referenced entities (Bug #10).
- Improved performance of the getter/setter Aspect. This also caused smaller memory requirements for tracking persistence context of entities.
- Rewrote persistence provider discovery to work the same as in JPA.
- Better support of the `isLoaded` methods.

## 0.9.10 - October 16, 2017
- Partial support for SPARQL result set mapping - VariableResult and ConstructorResult (Feature #7).
- Fixed issue with class discovery in JDK 9 (Bug #8, thanks to Yan Doroshenko).
- Log executed query to provide more auditing info.

## 0.9.9 - September 2, 2017
- Support for credentials specification for accessing remote RDF4J repositories.
- Reworked instance persist to prevent saving pending references before the target instances are saved.
This also fixes the problem of prematurely generated IDs.
- Reworked merge to have semantics more corresponding to JPA.
- Allow descriptor specification for TypedQuery results. This allows context to be specified for these results.
It also replaces the original possibility of setting context for query, which did not work anyway.

## 0.9.8 - June 26, 2017
- Added support for language tag specification of String attributes.

## 0.9.7 - May 27, 2017
- Fixed issue with handling cascading cycles (Bug #4).
- Fixed issue with parsing queries without spaces after variables (Bug #6).
- Fixed issue with loading unmapped properties in Sesame driver.
- Added support for namespace specification.
- Added support for generating subclasses in OWL2Java.
- Improved handling of query result sets.

## 0.9.6 - April 27, 2017
- Fixed issue with loading classes containing 'class' in name (Bug #5).
- Implemented support for `EntityListener`s.
- Escape string parameters in SPARQL native queries.

## 0.9.5 - March 22, 2017
- Fixed issue with mapped superclass referencing its descendants (Bug #3).
- Fixed issue with cloning content of singleton collections elements.
- Upgraded Sesame driver from Sesame API 2.8.9 to RDF4J 2.2.
- Mark transaction for rollback when exception is thrown in persistence context-related operations.

## 0.9.4 - March 10, 2017
- Fixed issue with merge overwriting values of references in managed objects (Bug #2).
- Aligned merge implementation with the JPA specification semantics.

## 0.9.3 - February 28,2017
- Fixed issue in which second level cache was being bypassed by instances loaded from repository.
- Create default collection instance (currently `ArrayList`, `HashSet`) when unable to clone the specified instance.
- Minor performance improvements.

## 0.9.2 - February 18, 2017
- Internal entity lifecycle callbacks support (Lifecycle callback methods declared directly in entity classes).

## 0.9.1 - January 3, 2017
- Fixed major memory leak in persistence context management.

## 0.9.0 - December 13, 2016
- Support for single inheritance.

## 0.8.8 - December 9, 2016
- Added support for datatype property collections in entities.

## 0.8.7 - November 3, 2016
- Added support for `Query.executeUpdate`, which executes SPARQL Update statements on the repository. (_This feature is not supported by OWL2Query, so it only applies to Sesame-backed applications._)

## 0.8.6 - September 30, 2016
- Fixed issue with merging changes into collections which do not support the  `clear` operation (e.g. singleton collections).

## 0.8.5 - September 26, 2016
- Upgraded to OWL API 4.2.4 - it fixes issues with missing import statements after ontology copy,
- Changed the way transactional snapshot are created in the OWL API driver.
- Refactored metamodel as a preparation for inheritance implementation. Use AspectJ 1.8.9.

## 0.8.4 - July 13, 2016
- Added support for named native queries. Declaration is similar to JPA, just use `@NamedNativeQuery` or `@NamedNativeQueries`
on an entity or its mapped superclass and the query will be available in the persistence unit.

## 0.8.3 - July 12, 2016
- Consistent handling of instances with the same identifier (i.e. the same individual). Only single instance with a given
identifier is allowed in one persistence context. But, persisting the same individual as instance of different classes in
different persistence contexts is possible.

## 0.8.2 - July 4, 2016
- Major improvement of instance loading in the Sesame driver
- Fixed bug in `CollectionChangeDetector`, which caused identical collection to behave as different (issue with element ordering)

## 0.8.1 - June 29, 2016
- More efficient handling of repository connections in the Sesame driver

## 0.8.0 - June 17, 2016
- Added support for Mapped superclass (`@MappedSuperclass` annotation)
- Do not allow non-entities in `EntityManager` operations (e.g. `persist`, `find`) - conforms to JPA

## 0.7.12 - June 8, 2016
- Fixed issue with updating value in context,
- Verify integrity constraints on merge

## 0.7.11 - June 6, 2016
- Generation of Vocabularies improvements - individuals, whole file processing.

## 0.7.10 - June 1, 2016
- Added support for URI-based types, i.e. now it is possible to use `@Types Set<URI> types` in entities

## 0.7.9 - May 22, 2016
- Fixed issue with running multiple persistence units in the same VM.

## 0.7.8 - May 16, 2016
- Implemented support for `EntityManager.unwrap`. Using unwrap, it is possible to get a hold of for example the Sesame 
    in-memory repository used by the Sesame driver (which is otherwise, due to the Sesame API architecture, inaccessible 
    through the `RepositoryProvider`.

## 0.7.7 - May 11, 2016
- Added the possibility to generate only vocabulary using OWL2Java, without the Java object model itself.

## 0.7.6 - April 15, 2016
- Added support for plain identifiers as values of Annotation and Object properties
    - E.g. when an entity has an object property pointing to another individual, but that individual is not mapped by any class in the object model, its URI can now be used as the property value

## 0.7.5 - March 31, 2016
- Use xsd:int for mapping Java Integer/int values to RDF. Most Java implementations (including JAXB) do it this way.

## 0.7.4 - March 21, 2016
- Implemented support for typed @Properties fields (i.e. @Properties field can now be a `Map<URI, Set<Object>>`, where values have corresponding types instead of plain strings)
