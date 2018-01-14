# JOPA - Release Notes

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
