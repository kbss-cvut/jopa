package cz.cvut.kbss.ontodriver.rdf4j.connector.init;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectorFactoryImpl;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.loader.DefaultContextInferenceStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.DefaultStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.GraphDBStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.StatementLoaderFactory;

/**
 * Builds factories for the driver.
 */
public class FactoryOfFactories {

    private final DriverConfiguration config;
    private final RepositoryConnectorInitializer connectorInitializer;

    public FactoryOfFactories(DriverConfiguration config) throws Rdf4jDriverException {
        this.config = config;
        this.connectorInitializer = new RepositoryConnectorInitializer(config);
        connectorInitializer.initializeRepository();
    }

    public ConnectorFactory createConnectorFactory() {
        return new ConnectorFactoryImpl(new StorageConnector(connectorInitializer));
    }

    public StatementLoaderFactory createStatementLoaderFactory() throws Rdf4jDriverException {
        if (config.is(Rdf4jConfigParam.INFERENCE_IN_DEFAULT_CONTEXT)) {
            return new DefaultContextInferenceStatementLoaderFactory();
        }
        if (GraphDBStatementLoaderFactory.isRepositoryGraphDB(connectorInitializer.getRepository())) {
            return new GraphDBStatementLoaderFactory();
        }
        return new DefaultStatementLoaderFactory();
    }
}
