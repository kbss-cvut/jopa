# JOPA - Change Log

### 2.6.1 - 2025-09-30

- **Experimental** implementation of optimized entity retrieval from query results (Enhancement #357).
    - Enabled on entity manager/entity manager factory level using the
      `cz.cvut.kbss.jopa.experimental.query.enableEntityLoadingOptimizer` property (see the `JOPAExperimentalProperties`
      class).
    - Works only for typed queries selecting an entity — they project a single variable, and the query result type is an
      entity type.
    - The optimization adds a triple pattern `?x ?y ?z` into the query, selecting all property values for the given
      subject, the query projection is then modified to `?x ?y ?z`. Query result rows are transformed to axioms and an
      entity is reconstructed from them.
    - Works for plural attributes as well - result rows are grouped until a single entity can be reconstructed from
      them.
    - This optimization should reduce the number of repository calls. A single query execution will be able to load the
      resulting entities (previously, each entity was loaded separately after the query execution). Note that
      references (i.e., relationship traversal) are still loaded by separate repository calls.
    - Current limitations:
        - Optimization is not applied if offset or limit are set on the query
        - Optimization is not applied if the query results are retrieved as a stream (`Query.getResultStream`)
        - Descriptor provided to the query is not applied to the entity loading - entity is reconstructed directly from
          query results.
    - If an IC violation is thrown (because the query does not differentiate asserted and inferred data), the entity is
      loaded using the usual mechanism which separates asserted and inferred data.
    - Since JOPA now uses a grammar-based SPARQL parser, it is stricter when handling queries. This brings the following
      **potential breaking changes**:
        - Positional parameters without a position number are not supported anymore (e.g., `SELECT ?x WHERE { ?x a $ }`
          must be changed to `SELECT ?x WHERE { ?x a $1 }`). On the other hand, the same positional parameter (same
          position number) can now be used multiple times in a query.
        - Parameters cannot be used in IRI string anymore, i.e., `<http://example.com/?1>` will not work
- Dependency updates: RDF4J 5.1.6.

### 2.6.0 - 2025-09-15

- Support multilingual RDF containers (i.e., multiple translations at the same position in an RDF container) (Enhancement #369).
- Sanitize invalid characters in generated Java names in OWL2Java (thanks to @lukaskabc for a PR).
- Dependency updates: RDF4J 5.1.5.

### 2.5.1 - 2025-08-21

- Fix an issue with translating SOQL to SPARQL when referencing a RDF container attribute (Bug #366).

### 2.5.0 - 2025-08-18

- Support `IRI` as Query parameters (Enhancement #354).
- Add entity class IRI to the static metamodel (Enhancement #360).
- Implement a plugin API - currently there is a plugin for the whole persistence unit lifecycle (Enhancement #361).
- Support generating Java `URI` constants into the `Vocabulary` file in OWL2Java.
- Make static metamodel generator a service discoverable using the Java Service Provider Interface.

### 2.4.4 - 2025-07-21

- Fix an issue with merging entities with unset/changed query attributes (Bug #351).
- Dependency updates: Jena 5.5.0.

### 2.4.3 - 205-07-14

- Fix an issue with parsing SPARQL with IRIs containing hashtag (Bug #348).

### 2.4.2 - 2025-07-07

- Fix an issue with handling `ORDER BY` in SOQL/Criteria API (Bug #343), default order is now `ASC`.
- Support RDF collections and OWL lists in SOQL/Criteria API (Enhancement #339).
- Natively handle `Locale` instances - store them as RDF simple literals (Enhancement #340).

### 2.4.1 - 2025-06-25

- Prevent race condition when getting objects from second-level cache (Bug #329).
- Improve cache hit/miss ration by using the correct descriptor type for plural attribute elements (Bug #333).
- Support RDF containers in SOQL (typically `MEMBER OF` clauses) (Enhancement #335).

## 2.4.0 - 2025-05-29

- Add support for a read-only `EntityManager` which can be used to optimize read-only transactions (Feature #84).
- Add support for static repository context declaration using the `@Context` annotation (Feature #104).
- Implement `includeExplicit` attribute of `@Inferred` - it is now possible to exclude explicit (asserted) values from
  attribute (Enhancement #190).
- Add support for an in-memory OWLAPI storage (Enhancement #330).
- Dependency updates: Jena 5.4.0, RDF4J 5.1.3.

## 2.3.1 - 2025-04-07

- Ensure change tracking proxies are unwrapped on entity detach.

## 2.3.0 - 2025-03-31

- Add support for generic abstract entity classes/mapped superclasses (Enhancement #310).
- Add Virtuoso OntoDriver (Feature #57).
- Fix an issue with RDF container type statement context (Bug #316).
- Dependency updates: RDF4J 5.1.2.

## 2.2.3 - 2025-03-21

- Fix a NPX when calling `getContexts` on RDF4J driver (Bug #311).
- Dependency updates: Jena 5.3.0.

## 2.2.2 - 2025-01-18

- Fix an issue with parsing SPARQL query parameters (Enhancement #294).
- Fix an issue with passing a lazy loading proxy to `EntityManager.remove` (Bug #306).
- Refactor test metamodel setup (GH #301).
- Allow using primitive types as mapped attribute types.
- Dependency updates: RDF4J 5.1.0.

## 2.2.1 - 2024-12-12

- Rename `MultilingualString` in OntoDriver to `Translations` to prevent confusion with `MultilingualString` from JOPA (
  GH #288).
- Fix a SPARQL query parsing issue (Bug #294).

## 2.2.0 - 2024-11-22

- Treat any non-entity type as immutable w.r.t. to change tracking so that we do not have to explicitly list them in
  code (GH #278).
- Allow projecting entity attributes from SOQL/Criteria API queries (Enhancement #277).
- Allow using `Object` as data property field type, resolve actual type from data (GH #283).
- Use lists for plural query attributes when field type is `Collection` (Enhancement #276).
- Fix lazy loading of plural query attributes (Bug #285).
- Ignore SPARQL comments when parsing native SPARQL queries (Bug #275).
- Fix Jena file storage statement removal issue (Bug #274).
- Dependency updates: RDF4J 5.0.3.

## 2.1.0 - 2024-10-16

- Add support for RDF containers (Task #52).
  [Wiki](https://github.com/kbss-cvut/jopa/wiki/Object-ontological-Mapping#collection-mapping) contains info on
  collection mapping in general.
- Add support for Jena TDB2 storage.
- Dependency updates: Jena 5.2.0, OWL API 5.5.1, SLF4J 2.0.16.

## 2.0.5 - 2024-09-03

- Modify target ontology selection in queries (use only query hints) to prevent RDF4J driver from spanning application
  log.

## 2.0.4 - 2024-08-15

- Fix an issue with cloning instances of `AbstractImmutableMap` (produced by `Map.of`) (Bug #264).
- Ensure changes done by `@PreUpdate` callback inherited by entity are propagated to repository (Bug #265).
- Dependency updates: RDF4J 5.0.2.

## 2.0.3 - 2024-07-22

- Fix an issue with generating static metamodel under JDK 21 (Bug #257).
- Support using URI/URL as data property values (as RDF simple literal) (Enhancement #256).
- Support target ontology query hint (Enhancement #245).
- Dependency updates: Jena 5.1.0, RDF4J 5.0.1.

## 2.0.2 - 2024-06-24

- Fix a NPX when calling `EntityManager.isInferred` with lazy loading proxy (Bug #252).
- Allow using multiple values for a query parameter that is written into SPARQL `VALUES` clause (Bug #246).
- Dependency updates: RDF4J 5.0.0.
    - Update note: Still compatible with server 4 and, with
      the [right configuration](https://github.com/kbss-cvut/jopa/releases/tag/v1.0.0),
      also 3. However, if you are passing repository configuration file to JOPA, make sure it uses
      the [new configuration vocabulary](https://rdf4j.org/documentation/reference/configuration/).

## 2.0.1 - 2024-06-17

- Proper implementation of `EntityManager.getReference` after 2.0.0 rewrite (Enhancement #233).
- Log JOPA version and build date on persistence unit startup (Enhancement #243).
- Prevent `AssertionError` on `EntityManager.flush` calls (Bug #240).
- Fix incorrect SOQL to SPARQL translation when traversing reference and using identifier (Bug #234).
- Fix issues with interaction of lazy loading with cascading (Bug #248).
- Dependency updates: RDF4J 4.3.12.

## 2.0.0 - 2024-05-27

- Move internal API from `jopa-api` to the `jopa-impl` module (Enhancement #146).
- Modify name resolution in OWL2Java, support prefixes so that terms are better disambiguated without appending the
  useless `_A` suffix if possible (Enhancement #85).
- Remove dependency on AspectJ by rewriting object change tracking and lazy loading (Enhancement #145, #231). See
  the [wiki](https://github.com/kbss-cvut/jopa/wiki/Change-Tracking-and-Lazy-Loading) for details.
- Add support for RDF collections (Enhancement #51).
- Dependency updates: Jena 5.0.0, RDF4J 4.3.11.
- Require Java 17.

## 1.2.2 - 2024-01-30

- Modify `DefaultClasspathScanner` to handle Spring Boot nested JAR introduced in 3.2.0 (Bug #227).

## 1.2.1 - 2024-01-22

- Unwrap `IndirectMultilingualString` instances when owner entity is detached from the persistence context.

## 1.2.0 - 2023-12-20

- Support data properties for referenced list values (Enhancement #209).
- Support using multilingual string as referenced list values (Enhancement #216).
- Fix inconsistent license information (GH #214).
- Dependency updates: Jena 4.10.0, RDF4J 4.3.8.

## 1.1.4 - 2023-10-25

- Support ignoring removals of inferred values on entity merge into persistence context (Enhancement #189).
- Dependency updates: RDF4J 4.3.7.

## 1.1.3 - 2023-09-22

- Fix a transaction isolation issue. Now when multiple transactions edit the same data, the one that commits last wins (
  same behavior as JPA, Bug #202).
- Fix an issue with missing interpretation of prefixes in the `@Sequence` annotation values (Bug #204).

## 1.1.2 - 2023-09-14

- Fix missing processing of connection pool configuration in RDF4J driver.
- Disambiguate classes of the same name in OWL2Java using a default suffix (Bug #199).
- Modify JOPA Maven plugin parameter mapping so that it does not confuse IDEs (GH #198).
- Provide access to entity class namespace detection (Enhancement #196).
- Dependency updates: RDf4J 4.3.6.

## 1.1.1 - 2023-08-23

- Fix a possible deadlock in case RDF4J's HTTP client connection pool is exhausted by concurrent requests (Bug #191).
- Introduce a marker `@Property` annotation used on all OWL property mapping annotations.
- Dependency updates: AspectJ 1.9.20, RDF4J 4.3.5.

## 1.1.0 - 2023-07-28

- Add support for MEMBER OF in SOQL/Criteria API (Enhancement #176).
- Support language matching in SOQL/Criteria API (Enhancement #161).
- Allow declaring multiple (or no) scan packages (Enhancement #185).
- Support both legacy and
  new [RDF4J repository config vocabulary](https://rdf4j.org/documentation/reference/configuration/#migrating-old-configurations).
- Dependency update: RDF4J 4.3.4, Jena 4.9.0.

## 1.0.0 - 2023-06-29

- Add support for disabling inference when loading an entity (e.g., when loaded with disable inference query hint) (
  Enhancement #144).
- Add API allowing to check whether an attribute value is inferred (Enhancement #141).
- Support multiple inheritance via interfaces in the object model (Enhancement #157).
- Add implementation of static metamodel generator (Feature #79).
- Rework annotation property value storage (Enhancement #175).
- Dependency updates: RDF4J 4.2.3, Jena 4.8.0, OWL API 5.5.0, AspectJ 1.9.17.

### Breaking Changes

- Removed deprecated Sesame driver (was replaced by RDF4J driver) (#120).
- Removed deprecated query execution ontology API from EntityManager/Query. Was replaced by
  a [query hint](https://github.com/kbss-cvut/jopa/wiki/Query-Hints).
- Reworked storage of annotation property values. From now on, String annotation property values are always stored as
  string.
  Previously, JOPA attempted to guess if the value was a URI/IRI by parsing the value, which lead to incorrect handling
  of values containing `:`.
  Now, only values of **type** URI/IRI (or entities) will be stored as individuals/resources.
- Require Java 11 or later.

## 0.22.2 - 2023-06-22

- Fix an issue with commutativity of AND in SOQL when selection by entity identifier is used (Bug #178).

## 0.22.1 - 2023-06-07

- Allow using SPARQL SELECT queries without WHERE keyword (Bug #165).
- Allow disabling generation of annotation fields and the `Thing` class in OWL2Java (Enhancement #170).
- Fix `simpleLiteral=true` not being honored when saving annotation property values (Bug #171).
- Fix an issue with parsing SOQL queries containing selection by identifier and attribute value.
- Fix selection by identifier in SPARQL when using the OWLAPI driver (was caused by missing support for VALUES).

## 0.22.0 - 2023-04-26

- Allow selecting entities by identifier in SOQL/Criteria API (Enhancement #138).
- Extend SOQL/Criteria API with additional functions - `lower`, `upper`, `length`, `abs`, `ceil`, `floor` (Enhancement
  #152).
- Fix an issue with interaction of lazy loading with inferred-valued attributes (Bug #150).
- Fix a possible NPX when unpersisted empty entity is referenced in another entity's equals/hashCode.

## 0.21.1 - 2023-03-21

- Allow using generic types as plural attribute value elements.

## 0.21.0 - 2023-03-06

- Support for mapping Java enums to ontological individuals (`owl:ObjectOneOf`) (Feature #60).
- Support for mapping Java enums via ordinals or strings (Enhancement #134).

## 0.20.2 - 2023-02-23

- Support automatic conversion of language-less strings to `LangString` attributes.
- Support using `LangString` as query parameters.
- Support joining over plural attributes in SOQL/Criteria API (Bug #135).
- Implement stricter cleanup after EMF close (Enhancement #136).

## 0.20.1 - 2023-02-07

- Allow building a restricted metamodel from a set of entity classes (will be used by static metamodel generator).

## 0.20.0 - 2023-01-26

- Allow editing inferred attributes (see Wiki for details) (Feature #121).
- Support for `IN`, `NOT LIKE`, `<>` (`!=`) operators in SOQL (and the corresponding operators in Criteria API). (
  Enhancement #123, #129).

## 0.19.3 - 2022-12-13

- Allow excluding `@OWLClass` annotated classes from persistence by using `NonEntity` (Enhancement #124).
- Update dependencies: SF4J 2.0.6, Logback 1.3.5, Mockito 4.9.0.

## 0.19.2 - 2022-11-10

- Fix issues in working with `LocalTime` values.

## 0.19.1 - 2022-11-08

- Fix issues in working with `BigInteger` and `BigDecimal` values.
- Fix a warning about illegal reflective operations on newer JDKs.

## 0.19.0 - 2022-10-26

- Add support for disabling inference via query hints (Enhancement #101).
- Allow registering custom attribute converters (Enhancement #118).
- Add RDF4J driver, deprecate Sesame driver (Task #75).
- Dependency updates: SLF4J 2.0.3, Logback 1.3.4.

## 0.18.7 - 2022-08-31

- Fix support for custom datatypes in Jena driver (Bug #113).
- Fix mapping enums to simple literals (Bug #111).
- Fix an issue with double-wrapped indirect collections when cloning singleton Maps (Bug #114).
- Dependency updates: RDF4J 3.7.7.

## 0.18.6 - 2022-05-09

- Automatically resolve inferred statements contexts for GraphDB (Enhancement #106).
- Allow configuring Sesame driver to load inferred statements from the default context (See wiki for details).
- Move supported identifier types constants from jopa-impl to jopa-api module.

## 0.18.5 - 2022-03-29

- Add support for special floating point numeric values NaN, -INF, INF (Bug #108).
- Dependency updates: RDF4J 3.7.6, SLF4J 1.7.36, Logback 1.2.11.

## 0.18.4 - 2022-03-22

- Make `LangString` extensible (need by JB4JSON-LD).
- Fix incorrect implementation of transformation to float in `DatatypeTransformer`.
- Dependency updates: OWL API 5.1.20.

## 0.18.3 - 2022-02-21

- Add a no-arg constructor to `LangString` so that it can be serialized/deserialized by JSON mapping libraries like
  Jackson.
- Fix a classpath scanning issue on Windows.
- Explicitly use UTF-8 encoding when reading mapping file.

## 0.18.2 - 2022-02-14

- Prevent attempts at using `rdf:type` as property mapping (`@Types` should be used instead).
- Allow using `LangString` as entity attribute type.

## 0.18.1 - 2022-01-16

- Support date/time value conversions in SPARQL result mapping.

## 0.18.0 - 2022-01-13

- Implemented full support for Java 8 date/time API. It is now the preferred way of representing temporal data (#95).
- Do not validate participation constraints on inferred attributes on persist (#98).
- Support mapping lexical form of literals with explicit datatype (#96).
- Allow retrieving entity classes mapping the specified class IRI from `Metamodel` (#99).
- Dependency updates - RDF4J 3.7.4, test dependencies, plugins.

## 0.17.2 - 2021-09-27

- Reload SPARQL-based attribute values when object is retrieved from cache to ensure up-to-date results (#93).
- Support referencing other entity attributes in SPARQL-based attribute queries (#94).
- Dependency updates - RDF4J 3.7.3, SLF4J 1.7.32, Logback 1.2.6 and others.

## 0.17.1 - 2021-09-13

- Switch back to the resurrected official [aspectj-maven-plugin](https://github.com/mojohaus/aspectj-maven-plugin) which
  allows running on JDK up to 16.
- Dependency updates: RDF4J 3.7.2, OWL API 5.1.19.

## 0.17.0 - 2021-06-29

- Add support for SPARQL-based entity attributes (Feature #65).
- Add support for Criteria API (Feature #20).
- Fix handling of ASK queries with prefixes in the Sesame Driver (Bug #89).
- Dependency updates: AspectJ 1.9.7, RDF4J 3.7.1.

## 0.16.5 - 2021-04-28

- Dependency updates: RDF4J 3.6.3.

## 0.16.4 - 2021-03-03

- [JOPA] Support for initialization of static metamodel (has to be created manually for now) (Feature #83).
- Fix issues with building and using the project on Windows (path-related).
- Dependency updates: RDF4J 3.6.0, switched to a different fork of the maven-aspectj-plugin allowing to build the
  project under JDK 11.

## 0.16.3 - 2021-02-04

- [JOPA] Fix issues with cached references becoming stale after merge (Bug #81).
- [JOPA] Fix missing support for Windows-like newline characters (CRLF) in queries (Bug #82).
- Fix project build issues on Windows.
- Simplify `PersistenceProvider` implementation discovery (based on JPA spec implementation now).
- Dependency updates: RDF4J 3.5.1, Jena 3.17.0.

## 0.16.2 - 2020-11-19

- [JOPA] Prioritize transient configuration over property mapping in setter/getter aspects (Bug #80).

## 0.16.1 - 2020-11-13

- [Jena OntoDriver] Support connecting to Jena Fuseki server (Feature #76).
- Dependency updates: RDF4J 3.4.4, OWL API 5.1.17.

## 0.16.0 - 2020-10-12

- Support using multiple contexts for entity/attribute (Enhancement #73).
- Dependency updates: RDF4J 3.4.3.

## 0.15.2 - 2020-09-23

- [JOPA] Fix handling of multilingual strings in annotation properties when target type is `Object`.
- Allow configuring whether `MultilingualString` should be preferred over `String` in situations like above (
  `cz.cvut.jopa.preferMultilingualString`).
- Minor code quality improvements.

## 0.15.1 - 2020-09-21

- [JOPA] Add support for using collections as query parameters (Enhancement #74).
- [Sesame OntoDriver] Retry connection to remote repository when configured to (Feature #72).
- Minor fixes in multilingual string handling.
- Dependency updates: RDF4J 3.4.1.

## 0.15.0 - 2020-09-06

- [JOPA] Add support for multilingual String attributes (Feature #59).
- Dependency updates: RDF4J 3.4.0, AspectJ 1.9.6.

## 0.14.7 - 2020-08-04

- Dependency updates: Jena 3.16.0, OWL API 5.1.16, RDF4J 3.3.0.
- Project configuration simplification.
- Minor fix of formatting `java.util.Date` query parameter values.

## 0.14.6 - 2020-06-26

- Add missing support for mapping `float` attributes.

## 0.14.5 - 2020-06-18

- [JOPA] Extend XSD constants.
- Dependency update - RDF4J 3.2.2.

## 0.14.4 - 2020-05-27

- [JOPA] Fix handling of objects with overridden `equals`/`hashCode` in `PendingReferenceRegistry`.

## 0.14.3 - 2020-05-20

- [OWLAPI OntoDriver, OWLAPI Utils] - Refactor common code in OWL API OntoDriver and OWL API Utils module. [Task #68]
- [OWL2Java] Support mapping **to** remote URLs in the IRI mapping file.
- [JOPA] Support `CollectionAttribute`s. [Task #56]
- [OWL2Java] Sort imported ontology IRIs to ensure stable ordering of generated vocabulary constants.
- Dependency updates - RDF4J 3.2.1, Jena 3.15.0, SLF4J 1.7.30.

## 0.14.2 - 2020-04-28

- [OWL2Java] Do not generate id, label, comment, types and properties fields in entities which already inherit them from
  parent. (Bug #66, thanks to @cristianolongo for reporting and providing a PR with fix)
- [OWL2Java] Ensure ontology IRI is output into generated vocabularies only once even in case of multiple imports.
- [JOPA] Add constants for all DC Terms properties and classes.
- Dependency updates - RDF4J 3.1.4, OWLAPI 5.1.14.

## 0.14.1 - 2020-03-22

- [JOPA] Support using Java 8 Date/Time API as query parameters.
- [OWL2Java] Log reasons why an ontology could not be imported for transformation.
- Dependency updates - RDF4J 3.1.2.

## 0.14.0 - 2020-03-02

- [JOPA] Implement basic support for the Semantic Object Query Language (SOQL), an object model-based query language. (
  Feature #19)
- [JOPA] Support annotation properties of type `Object`, which allow to accommodate both literals and URIs.
- Dependency updates - RDF4J 3.1.1.

## 0.13.5 - 2020-02-19

- [JOPA] Fix an issue with loading entity classes in Spring Boot projects, where classes are in a non-standard location
  in the JAR (Bug #62).

## 0.13.4 - 2020-02-12

- [JOPA] Support using entities as query parameters (Feature #61).
- [JOPA] Run IC validation on lazy loaded fetched attributes also in case no value is loaded.
- [OWLAPI OntoDriver] Search also in imported ontologies when loading data.
- Dependency updates - RDF4J 3.1.0, OWLAPI 5.1.13, Jena 3.14.0, AspectJ 1.9.5.

## 0.13.3 - 2019-11-08

- Upgraded to RDF4J 3.0.2 and OWLAPI 5.1.12.
- Fixed the issue with incorrect treatment of default context specification for attribute of an entity in a different
  context (Bug #58).

## 0.13.2 - 2019-10-21

- Upgraded to RDF4J 3.0.1.
- Fixed missing support for lexical form and simple literal plural attributes.
- Automatically generate `toString` method in OWL2Java model transformation.

## 0.13.1 - 2019-08-08

- Fixed incorrect context handling in `ObjectPropertyCollectionDescriptor`.

## 0.13.0 - 2019-07-15

- Added support for read-only literal lexical form access. This allows to load the lexical form of literals regardless
  of their type (Task #50).
- Added support for RDF *simple literals*, i.e. literals of type `xsd:string`. Simple literals allow to use strings
  without language tags (Task #53).
- Fixed handling of named graphs. Store property assertions in the subject's graph by default (Task #54).
- Fixed incorrect handling of repository credentials in the RDF4J driver.
- Added support for recursive entity descriptors.
- Updated drivers to always use language specified by assertions and removed global language setting from the drivers.
- Upgraded to OWLAPI 5.1.11, Jena 3.12.0 and RDF4J 2.5.3.

## 0.12.2 - 2019-05-28

- Do not generate error message when directory structure for OWL2Java already exists.
- Fixed issue with parent entity listeners/lifecycle callbacks not being called (Bug #49).
- Fixed problems with using Spring Boot Devtools with JOPA (classloader issues, Task #26).
- Added [SKOS](http://www.w3.org/TR/skos-reference) vocabulary constants.
- Upgraded to AspectJ 1.9.4, Jena 3.11.0 and RDF4J 2.5.2.

## 0.12.1 - 2019-03-26

- Generate ASCII-only identifier names (OWL2Java).
- Fixed issues with caching instances referencing partially loaded getReference results.
- Upgraded to Jena 3.10.0, OWLAPI 5.1.10 and RDF4J 2.5.0.

## 0.12.0 - 2019-02-27

- Implemented support for `EntityManager.getReference` (Feature #46).
- Prevent adding `null` into plural attribute collection when value cannot be loaded (Fix).
- Upgraded to RDF4J 2.4.5.

## 0.11.0 - 2019-01-15

- Added support for specifying repository config when creating embedded in-memory and native RDF4J repositories (Feature
  #41).
- Added supports for Java 8 Streams in the Query API (Feature #45).
- Fixed issues where aspects were not invoked for fields inherited from mapped superclasses.
- Fixed possible infinite loop caused by entity lifecycle callbacks/listeners modifying the entity itself.
- Upgraded to RDF4J 2.4.2.
- Removed several deprecated parts of the API.

## 0.10.8 - 2018-12-12

- Support for plural annotation properties (Feature #38).
- Fixed incorrect rdf:Property constant.
- Added definitions of a subset of the Dublin Core vocabulary.
- Fixed issues with support of polymorphism in entity attributes.

## 0.10.7 - 2018-11-01

- Added support for Java 8 Date/Time API (Feature #36).
- Fixed issue with caching instances of classes overriding `equals`/`hashCode`.
- Upgraded to AspectJ 1.9.2, Jena 3.9.0, OWL API 5.1.7 and RDF4J 2.4.1.

## 0.10.6 - 2018-09-25

- Support unbound variables in `VariableResult` in `SparqlResultMapping`.
- Fixed incorrect handling of language on attribute loading.
- More consistent handling of open/close persistence context.

## 0.10.5 - 2018-08-13

- Removed several unused files, libraries.
- Improved collection cloning.
- Fixed incorrect field cardinality specification in OWL2Java.
- Upgraded to OWLAPI 5.1.6.
- Added `aop.xml` so that [load time weaving](https://github.com/kbss-cvut/jopa/wiki/Maven-setup#load-time-weaving) can
  be used with JOPA.

## 0.10.4 - 2018-07-20

- Fixed a critical NPX issue caused by inherited indirect collections not being removed when an entity was detached from
  the persistence context.

## 0.10.3 - 2018-07-19

- Reduced memory footprint.
- Configurable generation of Javadoc from rdfs:comment annotations in OWL2Java (Enhancement #35).
- Entity classes generated by OWL2Java implement `Serializable`.
- Introduced dedicated classes with constants from RDF, RDFS and OWL namespaces into the API.
- Deprecated `CommonVocabulary`, use `RDF`, `RDFS`, `OWL` and `XSD` classes instead.

## 0.10.2 - 2018-06-18

- Fixed bug SPARQL query parser which prevent the use of property paths together with variables.
- Added support for setting the in-memory storage in Jena and RDF4J driver (Enhancement #32).
- Working on Github Wiki.

## 0.10.1 - 2018-05-20

- Fixed bug where old object property assertion was not removed on merge (Bug #33).
- Allow to reload file-based storage in OntoDriver (Enhancement #33).
    - Use the `ReloadableDataSource`, which can be unwrapped from an `EntityManagerFactory`.
- Fixes in the Jena driver. Support for consistency checks.
- Do not mark transaction for rollback on `NoResultException` and `NoUniqueResultException`.
  This improves compatibility with Spring declarative transaction support (`jopa-spring-transaction`).
- Library updates (AspectJ 1.9.1, RDF4J 2.3.1).
- Support for anonymous ontologies in OWLAPI driver.

## 0.10.0 - 2018-04-11

- Jena OntoDriver implementation (Enhancement #18).
    - In-memory, File and TDB storage are supported.

## 0.9.16 - 2018-03-29

- Fixed bug in cloning instances with circular dependencies (using collections).
- Updated the compiler and AspectJ plugins.

## 0.9.15 - 2018-02-21

- Fixed dependency conflict in JOPA Maven plugin (Bug #29).

## 0.9.14 - 2018-02-20

- Fixed incorrect implementation of `contains` in Sesame driver.
- Minor memory consumption optimizations.
- Fixed volatile order of generated vocabulary elements (Issue #28).

## 0.9.13 - 2018-01-14

- Fixed issue with OWL2Java silently ignoring missing imports (Bug #22).
- Upgraded to OWLAPI 5.1.3. Upgraded to OWL2Query 0.5.0.
- Unified OWLAPI version in all project submodules.
- Switched to Openllet in tests (it supports OWLAPI 5).

## 0.9.12 - 2017-12-04

- Rewrote em.refresh to correspond to JPA behavior (Issue #17).
- Upgraded to the latest RDF4J (2.2.4).
- Generate class names compliant with Java naming conventions (OWL2Java).
- Added support for untyped query parameters to Query API (necessary for `LIMIT` and `OFFSET` support).
- Also added support for `Query.setFirstResult`.

## 0.9.11 - 2017-11-14

- Finished implementation of support for result set mapping - EntityResult (Feature #7).
- Fixed issue with PostLoad lifecycle callbacks and listeners not being called for referenced entities (Bug #10).
- Improved performance of the getter/setter Aspect. This also caused smaller memory requirements for tracking
  persistence context of entities.
- Rewrote persistence provider discovery to work the same as in JPA.
- Better support of the `isLoaded` methods.

## 0.9.10 - 2017-10-16

- Partial support for SPARQL result set mapping - VariableResult and ConstructorResult (Feature #7).
- Fixed issue with class discovery in JDK 9 (Bug #8, thanks to Yan Doroshenko).
- Log executed query to provide more auditing info.

## 0.9.9 - 2017-09-02

- Support for credentials specification for accessing remote RDF4J repositories.
- Reworked instance persist to prevent saving pending references before the target instances are saved.
  This also fixes the problem of prematurely generated IDs.
- Reworked merge to have semantics more corresponding to JPA.
- Allow descriptor specification for TypedQuery results. This allows context to be specified for these results.
  It also replaces the original possibility of setting context for query, which did not work anyway.

## 0.9.8 - 2017-06-26

- Added support for language tag specification of String attributes.

## 0.9.7 - 2017-05-27

- Fixed issue with handling cascading cycles (Bug #4).
- Fixed issue with parsing queries without spaces after variables (Bug #6).
- Fixed issue with loading unmapped properties in Sesame driver.
- Added support for namespace specification.
- Added support for generating subclasses in OWL2Java.
- Improved handling of query result sets.

## 0.9.6 - 2017-04-27

- Fixed issue with loading classes containing 'class' in name (Bug #5).
- Implemented support for `EntityListener`s.
- Escape string parameters in SPARQL native queries.

## 0.9.5 - 2017-03-22

- Fixed issue with mapped superclass referencing its descendants (Bug #3).
- Fixed issue with cloning content of singleton collections elements.
- Upgraded Sesame driver from Sesame API 2.8.9 to RDF4J 2.2.
- Mark transaction for rollback when exception is thrown in persistence context-related operations.

## 0.9.4 - 2017-03-10

- Fixed issue with merge overwriting values of references in managed objects (Bug #2).
- Aligned merge implementation with the JPA specification semantics.

## 0.9.3 - 2017-02-28

- Fixed issue in which second level cache was being bypassed by instances loaded from repository.
- Create default collection instance (currently `ArrayList`, `HashSet`) when unable to clone the specified instance.
- Minor performance improvements.

## 0.9.2 - 2017-02-18

- Internal entity lifecycle callbacks support (Lifecycle callback methods declared directly in entity classes).

## 0.9.1 - 2017-01-03

- Fixed major memory leak in persistence context management.

## 0.9.0 - 2016-12-13

- Support for single inheritance.

## 0.8.8 - 2016-12-09

- Added support for datatype property collections in entities.

## 0.8.7 - 2016-11-03

- Added support for `Query.executeUpdate`, which executes SPARQL Update statements on the repository. (_This feature is
  not supported by OWL2Query, so it only applies to Sesame-backed applications._)

## 0.8.6 - 2016-09-30

- Fixed issue with merging changes into collections which do not support the  `clear` operation (e.g. singleton
  collections).

## 0.8.5 - 2016-09-26

- Upgraded to OWL API 4.2.4 - it fixes issues with missing import statements after ontology copy,
- Changed the way transactional snapshot are created in the OWL API driver.
- Refactored metamodel as a preparation for inheritance implementation. Use AspectJ 1.8.9.

## 0.8.4 - 2016-07-13

- Added support for named native queries. Declaration is similar to JPA, just use `@NamedNativeQuery` or
  `@NamedNativeQueries`
  on an entity or its mapped superclass and the query will be available in the persistence unit.

## 0.8.3 - 2016-07-12

- Consistent handling of instances with the same identifier (i.e. the same individual). Only single instance with a
  given
  identifier is allowed in one persistence context. But persisting the same individual as instance of different classes
  in
  different persistence contexts is possible.

## 0.8.2 - 2016-07-04

- Major improvement of instance loading in the Sesame driver
- Fixed bug in `CollectionChangeDetector`, which caused identical collection to behave as different (issue with element
  ordering)

## 0.8.1 - 2016-06-29

- More efficient handling of repository connections in the Sesame driver

## 0.8.0 - 2016-06-17

- Added support for Mapped superclass (`@MappedSuperclass` annotation)
- Do not allow non-entities in `EntityManager` operations (e.g. `persist`, `find`) - conforms to JPA

## 0.7.12 - 2016-06-08

- Fixed issue with updating value in context,
- Verify integrity constraints on merge

## 0.7.11 - 2016-06-06

- Generation of Vocabularies improvements - individuals, whole file processing.

## 0.7.10 - 2016-06-01

- Added support for URI-based types, i.e. now it is possible to use `@Types Set<URI> types` in entities

## 0.7.9 - 2016-05-22

- Fixed issue with running multiple persistence units in the same VM.

## 0.7.8 - 2016-05-16

- Implemented support for `EntityManager.unwrap`. Using unwrap, it is possible to get a hold of for example the Sesame
  in-memory repository used by the Sesame driver (which is otherwise, due to the Sesame API architecture, inaccessible
  through the `RepositoryProvider`.

## 0.7.7 - 2016-05-11

- Added the possibility to generate only vocabulary using OWL2Java, without the Java object model itself.

## 0.7.6 - 2016-04-15

- Added support for plain identifiers as values of Annotation and Object properties
    - E.g. when an entity has an object property pointing to another individual, but that individual is not mapped by
      any class in the object model, its URI can now be used as the property value

## 0.7.5 - 2016-03-31

- Use xsd:int for mapping Java Integer/int values to RDF. Most Java implementations (including JAXB) do it this way.

## 0.7.4 - 2016-03-21

- Implemented support for typed @Properties fields (i.e. @Properties field can now be a `Map<URI, Set<Object>>`, where
  values have corresponding types instead of plain strings)
