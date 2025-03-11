package cz.cvut.kbss.ontodriver.virtuoso;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.ConfigurationParameter;
import cz.cvut.kbss.ontodriver.config.DriverConfigParam;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.ConnectionListener;
import cz.cvut.kbss.ontodriver.rdf4j.Rdf4jAdapter;
import cz.cvut.kbss.ontodriver.rdf4j.Rdf4jConnection;
import cz.cvut.kbss.ontodriver.rdf4j.Rdf4jContainers;
import cz.cvut.kbss.ontodriver.rdf4j.Rdf4jLists;
import cz.cvut.kbss.ontodriver.rdf4j.Rdf4jProperties;
import cz.cvut.kbss.ontodriver.rdf4j.Rdf4jTypes;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.config.RuntimeConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactory;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.loader.StatementLoaderFactory;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

class VirtuosoDriver implements AutoCloseable, ConnectionListener<Rdf4jConnection> {

    private static final List<ConfigurationParameter> CONFIGS = List
            .of(DriverConfigParam.AUTO_COMMIT, Rdf4jConfigParam.LOAD_ALL_THRESHOLD,
                    Rdf4jConfigParam.RECONNECT_ATTEMPTS,
                    Rdf4jConfigParam.CONNECTION_REQUEST_TIMEOUT);

    private final DriverConfiguration configuration;
    private boolean open;
    private final ConnectionFactory connectionFactory;
    private final StatementLoaderFactory statementLoaderFactory;

    private final Set<Rdf4jConnection> openConnections = new HashSet<>();

    VirtuosoDriver(OntologyStorageProperties storageProperties,
                   Map<String, String> properties) throws VirtuosoDriverException {
        assert storageProperties != null;
        assert properties != null;

        this.configuration = new DriverConfiguration(storageProperties);
        configuration.addConfiguration(properties, CONFIGS);
        final VirtuosoFactoryOfFactories factory = new VirtuosoFactoryOfFactories(configuration);
        this.connectionFactory = factory.createConnectorFactory();
        this.statementLoaderFactory = factory.createStatementLoaderFactory();
        this.open = true;
    }

    Connection acquireConnection() {
        assert open;
        final RuntimeConfiguration config = new RuntimeConfiguration(configuration);
        config.setStatementLoaderFactory(statementLoaderFactory);
        final Rdf4jAdapter adapter = new Rdf4jAdapter(connectionFactory.createStorageConnection(), config);
        final Rdf4jConnection c = new Rdf4jConnection(adapter);
        c.setLists(new Rdf4jLists(adapter, c::ensureOpen, c::commitIfAuto));
        c.setTypes(new Rdf4jTypes(adapter, c::ensureOpen, c::commitIfAuto));
        c.setProperties(new Rdf4jProperties(adapter, c::ensureOpen, c::commitIfAuto));
        c.setContainers(new Rdf4jContainers(adapter, c::ensureOpen, c::commitIfAuto));
        openConnections.add(c);
        c.setListener(this);
        return c;
    }


    @Override
    public void connectionClosed(Rdf4jConnection connection) {
        if (connection == null) {
            return;
        }
        openConnections.remove(connection);
    }

    @Override
    public void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        try {
            for (Rdf4jConnection c : openConnections) {
                c.removeListener();
                c.close();
            }
            connectionFactory.close();
        } catch (OntoDriverException e) {
            throw e;
        } catch (Exception e) {
            throw new Rdf4jDriverException(e);
        } finally {
            this.open = false;
        }
    }
}
