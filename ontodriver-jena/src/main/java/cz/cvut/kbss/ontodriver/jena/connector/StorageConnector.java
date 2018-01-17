package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Closeable;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;

public interface StorageConnector extends Closeable {

    /**
     * Begins a transaction.
     */
    void begin();

    /**
     * Commits the current transaction.
     */
    void commit();

    /**
     * Rolls back the current transaction.
     */
    void rollback();
}
