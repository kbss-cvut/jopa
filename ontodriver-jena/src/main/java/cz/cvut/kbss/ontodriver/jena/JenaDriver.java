package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfigParam;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.config.ConfigurationParameter;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.config.Constants;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.connector.*;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.util.ConnectionListener;

import java.util.*;

class JenaDriver implements Closeable, ConnectionListener {

    private static final List<ConfigurationParameter> CONFIGS = Arrays
            .asList(DriverConfigParam.AUTO_COMMIT, DriverConfigParam.ONTOLOGY_LANGUAGE, DriverConfigParam.REASONER_FACTORY_CLASS,
                    JenaConfigParam.ISOLATION_STRATEGY, JenaConfigParam.STORAGE_TYPE,
                    JenaConfigParam.TREAT_DEFAULT_GRAPH_AS_UNION);

    private volatile boolean open;

    private final DriverConfiguration configuration;
    private final ConnectorFactory connectorFactory;

    private final Set<JenaConnection> openConnections;

    private boolean autoCommit;

    JenaDriver(OntologyStorageProperties storageProperties, Map<String, String> properties) {
        assert properties != null;
        this.configuration = new DriverConfiguration(storageProperties);
        CONFIGS.stream().filter(c -> properties.containsKey(c.toString()))
               .forEach(c -> configuration.setProperty(c, properties.get(c.toString())));
        this.connectorFactory = buildConnectorFactory(properties);
        this.openConnections = Collections.synchronizedSet(new HashSet<>());
        this.autoCommit = configuration.isSet(DriverConfigParam.AUTO_COMMIT) ? configuration.is(DriverConfigParam.AUTO_COMMIT) :
                Constants.DEFAULT_AUTO_COMMIT;
        this.open = true;
    }

    private ConnectorFactory buildConnectorFactory(Map<String, String> properties) {
        final String isolationStrategy = configuration
                .getProperty(JenaConfigParam.ISOLATION_STRATEGY, Constants.DEFAULT_ISOLATION_STRATEGY);
        if (configuration.isSet(DriverConfigParam.REASONER_FACTORY_CLASS)) {
            // Once reasoner factory is set, this takes precedence, because only this factory is able to provide
            // proper reasoning support
            return new InferenceConnectorFactory(configuration, properties);
        }
        switch (isolationStrategy) {
            case JenaOntoDriverProperties.READ_COMMITTED:
                return new ReadCommittedConnectorFactory(configuration);
            case JenaOntoDriverProperties.SNAPSHOT:
                return new SnapshotConnectorFactory(configuration);
            default:
                throw new IllegalArgumentException("Unsupported transaction isolation strategy " + isolationStrategy);
        }
    }

    JenaConnection acquireConnection() {
        ensureOpen();
        final StorageConnector connector = connectorFactory.createConnector();
        final JenaAdapter adapter = new JenaAdapter(connector, connectorFactory.createInferredConnector(connector));
        final JenaConnection connection = new JenaConnection(adapter);
        connection.registerListener(this);
        connection.setAutoCommit(autoCommit);
        openConnections.add(connection);
        return connection;
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("Driver is closed.");
        }
    }

    @Override
    public void connectionClosed(JenaConnection connection) {
        openConnections.remove(connection);
    }

    synchronized void reloadStorage() throws JenaDriverException {
        ensureOpen();
        try {
            connectorFactory.reloadStorage();
        } catch (IllegalStateException e) {
            throw new JenaDriverException(e);
        }
    }

    @Override
    public synchronized void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        for (JenaConnection connection : openConnections) {
            connection.close();
        }
        connectorFactory.close();
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }
}
