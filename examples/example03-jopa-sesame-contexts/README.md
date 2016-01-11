# JOPA Example 03 - JOPA + Sesame + Contexts

This example shows usage of JOPA with a Sesame storage exploiting Sesame contexts - *named graphs* in RDF[1].

### Features

Main JOPA features shown in this demo include:

- Storage contexts support

#### Storage Contexts

Storage contexts (RDF named graphs) are a powerful way of structuring data in the storage. Sesame does support this feature
both in its native and in-memory storage.

In Sesame, not specifying context means working with the default context (graph), which is a union of all named contexts
in the storage. Therefore, it is usually the easiest way of working with the storage, especially when only reading the data.
However, sometimes it could be necessary to keep data in separate contexts and maintain them.

JOPA supports working with storage contexts on the *EntityManager* level. Currently, this support is realized via passing
descriptors to entity manager operations like `persist`, `find`, and `merge`. The descriptors may specify context URIs
for the whole instance and/or for each attribute separately.

##### Descriptors

Descriptors are used to specify contexts for entities. An `EntityDescriptor` specifies context for an entity instance. If
attribute contexts are not specified explicitly, the entity contexts applies to them as well, i.e. if the entity is context `A`,
then all its attributes are assumed to be in `A` as well.

Attribute contexts can be specified for entity, for primitive attributes, a `FieldDescriptor` is used. If an attribute is
also an entity, an `EntityDescriptor` should be used.

Setting the context URI to `null` means that the default context will be used.

## Persistence Setup

The persistence is set up in `cz.cvut.kbss.jopa.example03.PersistenceFactory`. Sesame native storage expects a particular
path to the storage, which looks like `/path/to/storage/repositories/repository-id`, where `path/to/storage/` is a
path in the file system and `repository-id` is id of the repository itself.

## Running the Demo

To run the demo, `mvn exec:java` can be used. The application first checks whether the storage already exists and if so,
it deletes it. This is to prevent integrity constraints violation when the data are persisted to the storage.

## References

[1] Jeremy J. Carroll, Christian Bizer, Patrick Hayes and Patrick Stickler: Named Graphs, Provenance and Trust.
    [http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.1.2197&rep=rep1&type=pdf](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.1.2197&rep=rep1&type=pdf)