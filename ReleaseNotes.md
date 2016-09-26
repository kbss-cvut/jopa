# JOPA - Release Notes

## 0.8.5 - September 26, 2016
- Upgraded to OWL API 4.2.4 - it fixes issues with missing import statements after ontology copy,
- Changed the way transactional snapshot are created in the OWL API driver.

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
