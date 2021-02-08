package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverInitializationException;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.sparql.core.Transactional;

import java.util.List;

/**
 * Represents underlying Jena storage.
 */
interface Storage {

    /**
     * Synchronize changes with the storage (e.g., disk).
     * <p>
     * Does nothing by default.
     *
     * @throws JenaDriverException Indicates that changes could not be written out
     */
    default void writeChanges() throws JenaDriverException {
    }

    /**
     * Gets a transactional representation of the underlying storage.
     * <p>
     * The results can be used by the {@link org.apache.jena.system.Txn} utility class.
     *
     * @return Jena {@code Transactional} instance
     */
    Transactional getTransactional();

    /**
     * Gets the dataset to which this instance is connected.
     *
     * @return Jena {@code Dataset}
     */
    Dataset getDataset();

    /**
     * Gets the default graph from this storage's dataset.
     *
     * @return Default graph
     */
    Model getDefaultGraph();

    /**
     * Gets a named graph with the specified identifier.
     *
     * @param ctx Context identifier
     * @return Named graph
     */
    Model getNamedGraph(String ctx);

    /**
     * Begins a transaction.
     *
     * @param readWrite Transaction read/write mode
     */
    void begin(ReadWrite readWrite);

    /**
     * Commits the current transaction.
     */
    void commit();

    /**
     * Rolls back the current transaction.
     */
    void rollback();

    /**
     * Closes this storage.
     */
    void close();

    /**
     * Adds the specified statements to the specified context (can be {@code null}).
     *
     * @param statements Statements to add
     * @param context    Context identifier, possibly {@code null} indicating default context
     */
    void add(List<Statement> statements, String context);

    /**
     * Removes the specified statements from the specified context (can be {@code null}).
     *
     * @param statements Statements to remove
     * @param context    Context identifier, possibly {@code null} indicating default context
     */
    void remove(List<Statement> statements, String context);

    /**
     * Removes the specified statements from the specified context (can be {@code null}).
     *
     * @param iterator Statement iterator
     * @param context  Context identifier, possibly {@code null} indicating default context
     */
    void remove(StmtIterator iterator, String context);

    /**
     * Creates a query execution which can be run.
     *
     * @param query Query to prepare execution for
     * @return {@code QueryExecution}
     */
    QueryExecution prepareQuery(Query query);

    /**
     * Executes the specified SPARQL update.
     *
     * @param update SPARQL update to execute
     */
    void executeUpdate(String update);

    /**
     * Reloads data from the underlying storage (if applicable).
     * <p>
     * Default implementation does nothing.
     */
    default void reload() {
    }

    /**
     * Sets the dataset on this storage.
     * <p>
     * Note that by default this method throws {@link UnsupportedOperationException}, because such an operation is
     * supported only by the in-memory storage.
     *
     * @param dataset The new dataset
     */
    default void setDataset(Dataset dataset) {
        throw new UnsupportedOperationException("Cannot set dataset on storage of type " + getClass().getSimpleName());
    }

    /**
     * Creates a storage accessor according to the specified configuration.
     *
     * @param configuration Access configuration
     * @return Storage accessor instance
     * @throws OntoDriverInitializationException When storage type is not supported
     */
    static Storage create(DriverConfiguration configuration) {
        final String type = configuration.getProperty(JenaConfigParam.STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        switch (type) {
            case JenaOntoDriverProperties.IN_MEMORY:
                return new MemoryStorage(configuration);
            case JenaOntoDriverProperties.FILE:
                return new FileStorage(configuration);
            case JenaOntoDriverProperties.TDB:
                return new TDBStorage(configuration);
            case JenaOntoDriverProperties.FUSEKI:
                return new FusekiStorage(configuration);
            case JenaOntoDriverProperties.SDB:
                throw new UnsupportedOperationException("Not implemented, yet.");
            default:
                throw new OntoDriverInitializationException("Unsupported storage type '" + type + "'.");
        }
    }
}
