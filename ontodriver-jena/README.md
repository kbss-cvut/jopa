# Jena OntoDriver

Allows to connect to Jena-based storage.

Currently supported storage types:

* In-memory (`in-memory`)
* File (`file`)
* TDB (`tdb`)

The storage type is configured using the `cz.cvut.kbss.ontodriver.jena.storage` property.

_SDB is not planned, as its development has been discontinued in favor of TDB._

### Isolation Strategies

The driver supports two types of transaction isolation strategies:

* Read committed (`read-committed`)
* Snapshot (`snapshot`)

They are configured using the `cz.cvut.kbss.ontodriver.jena.isolation` parameter.

##### Read committed

This strategy uses a shared connector for all read operations. Writing is done into a local change-tracking model.
Commit then writes the changes into the central connector.

This strategy is more memory efficient, but it provides lesser isolation between concurrent transactions.

##### Snapshot

This strategy creates a snapshot of the current storage state when a transaction begins. All subsequent reads/writes are
done on this snapshot. In addition, the writes are tracked in a local model. On commit, these changes are propagated
into the shared connector.

This puts more demand on the system memory, as the storage may be copied multiple times. However, it provides much higher
level of isolation for concurrent access.

**Note:** This strategy is by default used when inference is configured. This is a must have to allow access to transactional
changes in the reasoning process. Therefore, whenever a `cz.cvut.jopa.reasonerFactoryClass` parameter value
is passed in configuration to the driver, it automatically uses snapshot-based transaction isolation strategy, regardless
of any direct configuration of the transaction isolation strategy.


### Default Graph Treatment

Most storage treat the default graph as a union of all named graphs in the dataset. Jena, on the other hand, uses
the default graph as another, IRI-less, graph in the dataset. To get the union-based behavior, use `cz.cvut.kbss.ontodriver.jena.default_graph_as_union`.

All configuration parameters and values are declared and documented in `cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties`.
