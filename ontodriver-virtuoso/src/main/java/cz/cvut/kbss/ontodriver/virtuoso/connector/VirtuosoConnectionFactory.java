package cz.cvut.kbss.ontodriver.virtuoso.connector;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactory;
import cz.cvut.kbss.ontodriver.rdf4j.connector.RepoConnection;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StorageConnection;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.common.transaction.IsolationLevel;
import org.eclipse.rdf4j.repository.Repository;

public class VirtuosoConnectionFactory implements ConnectionFactory {

    private boolean open = true;

    private final IsolationLevel txIsolationLevel;

    public VirtuosoConnectionFactory(IsolationLevel txIsolationLevel) {this.txIsolationLevel = txIsolationLevel;}


    @Override
    public RepoConnection createStorageConnection() {
        if (!open) {
            throw new IllegalStateException("The factory is closed!");
        }
        return new StorageConnection(new VirtuosoConnectionProvider(), txIsolationLevel);
    }

    @Override
    public void close() throws OntoDriverException {

    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void setRepository(Repository repository) throws Rdf4jDriverException {

    }
}
