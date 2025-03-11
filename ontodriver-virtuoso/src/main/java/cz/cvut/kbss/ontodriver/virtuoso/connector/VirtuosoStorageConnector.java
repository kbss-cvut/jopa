package cz.cvut.kbss.ontodriver.virtuoso.connector;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.Wrapper;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.config.Constants;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jOntoDriverProperties;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Rdf4jConnectionProvider;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.virtuoso.VirtuosoDriverException;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import virtuoso.rdf4j.driver.VirtuosoRepository;

public class VirtuosoStorageConnector implements Closeable, Rdf4jConnectionProvider {

    private static final Logger LOG = LoggerFactory.getLogger(VirtuosoStorageConnector.class);

    private final DriverConfiguration configuration;
    private final int maxReconnectAttempts;

    private boolean open;
    private Repository repository;

    public VirtuosoStorageConnector(DriverConfiguration config) throws VirtuosoDriverException {
        this.configuration = config;
        this.maxReconnectAttempts = resolveMaxReconnectAttempts(config);
    }

    private static int resolveMaxReconnectAttempts(DriverConfiguration config) throws VirtuosoDriverException {
        final int attempts = config.getProperty(Rdf4jConfigParam.RECONNECT_ATTEMPTS, Constants.DEFAULT_RECONNECT_ATTEMPTS_COUNT);
        if (attempts < 0) {
            throw new VirtuosoDriverException(
                    "Invalid value of configuration parameter " + Rdf4jOntoDriverProperties.RECONNECT_ATTEMPTS +
                            ". Must be a non-negative integer.");
        }
        return attempts;
    }

    public void initializeRepository() {
        final String serverUri = configuration.getStorageProperties().getPhysicalURI().toString();
        LOG.debug("Initializing connector to repository at {}", serverUri);
        final String username = configuration.getStorageProperties().getUsername();
        final String password = configuration.getStorageProperties().getPassword();
        this.repository = new VirtuosoRepository(serverUri, username, password);
        this.open = true;
    }

    @Override
    public RepositoryConnection acquireConnection() throws Rdf4jDriverException {
        verifyOpen();
        LOG.trace("Acquiring repository connection.");
        return acquire(1);
    }

    private void verifyOpen() {
        if (!open) {
            throw new IllegalStateException("Connector is not open.");
        }
    }

    private RepositoryConnection acquire(int attempts) throws Rdf4jDriverException {
        try {
            return repository.getConnection();
        } catch (RepositoryException e) {
            if (attempts < maxReconnectAttempts) {
                LOG.warn("Unable to acquire repository connection. Error is: {}. Retrying...", e.getMessage());
                return acquire(attempts + 1);
            }
            LOG.error("Threshold of failed connection acquisition attempts reached, throwing exception.");
            throw new Rdf4jDriverException(e);
        }
    }

    @Override
    public ValueFactory getValueFactory() {
        verifyOpen();
        return repository.getValueFactory();
    }

    @Override
    public <T> T unwrap(Class<T> cls) throws OntoDriverException {
        verifyOpen();
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        }
        if (cls.isAssignableFrom(repository.getClass())) {
            return cls.cast(repository);
        }
        if (repository instanceof Wrapper) {
            return ((Wrapper) repository).unwrap(cls);
        }
        throw new VirtuosoDriverException("No class of type " + cls + " found.");
    }

    @Override
    public void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        try {
            repository.shutDown();
        } catch (RuntimeException e) {
            throw new VirtuosoDriverException("Exception caught when closing repository connector.", e);
        } finally {
            this.open = false;
        }
    }

    @Override
    public boolean isOpen() {
        return open;
    }
}
