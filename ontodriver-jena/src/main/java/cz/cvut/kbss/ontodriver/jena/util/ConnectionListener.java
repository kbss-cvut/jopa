package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.ontodriver.jena.JenaConnection;

@FunctionalInterface
public interface ConnectionListener {

    /**
     * Invoked when a connection is closed.
     *
     * @param connection The closed connection
     */
    void connectionClosed(JenaConnection connection);
}
