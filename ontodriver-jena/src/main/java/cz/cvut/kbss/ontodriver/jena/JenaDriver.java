package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.ConfigParam;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.config.ConfigurationParameter;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.config.Constants;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.jena.connector.ReadCommittedConnectorFactory;
import cz.cvut.kbss.ontodriver.jena.connector.SnapshotConnectorFactory;
import cz.cvut.kbss.ontodriver.jena.util.ConnectionListener;

import java.util.*;

public class JenaDriver implements Closeable, ConnectionListener {

    private static final List<ConfigurationParameter> CONFIGS = Arrays
            .asList(ConfigParam.AUTO_COMMIT, ConfigParam.ONTOLOGY_LANGUAGE,
                    JenaConfigParam.ISOLATION_STRATEGY, JenaConfigParam.STORAGE_TYPE,
                    JenaConfigParam.TREAT_DEFAULT_GRAPH_AS_UNION);

    private volatile boolean open;

    private final Configuration configuration;
    private final ConnectorFactory connectorFactory;

    private final Set<JenaConnection> openConnections;

    private boolean autoCommit;

    JenaDriver(OntologyStorageProperties storageProperties, Map<String, String> properties) {
        assert properties != null;
        this.configuration = new Configuration(storageProperties);
        CONFIGS.stream().filter(c -> properties.containsKey(c.toString()))
               .forEach(c -> configuration.setProperty(c, properties.get(c.toString())));
        this.connectorFactory = buildConnectorFactory();
        this.openConnections = Collections.synchronizedSet(new HashSet<>());
        this.autoCommit = configuration.isSet(ConfigParam.AUTO_COMMIT) ? configuration.is(ConfigParam.AUTO_COMMIT) :
                Constants.DEFAULT_AUTO_COMMIT;
        this.open = true;
    }

    private ConnectorFactory buildConnectorFactory() {
        final String isolationStrategy = configuration
                .getProperty(JenaConfigParam.ISOLATION_STRATEGY, Constants.DEFAULT_ISOLATION_STRATEGY);
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
        final JenaAdapter adapter = new JenaAdapter(connectorFactory.createConnector());
        final JenaConnection connection = new JenaConnection(adapter);
        connection.registerListener(this);
        connection.setAutoCommit(autoCommit);
        openConnections.add(connection);
        return connection;
    }

    @Override
    public void connectionClosed(JenaConnection connection) {
        openConnections.remove(connection);
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
