package cz.cvut.kbss.jopa.accessors;

import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.Connection;

public interface StorageAccessor extends Closeable {

    /**
     * Acquires a connection to the underlying ontology driver. </p>
     *
     * @return Connection to the storage
     * @throws StorageAccessException If an error occurs when acquiring connection from the underlying driver
     */
    Connection acquireConnection();
}
