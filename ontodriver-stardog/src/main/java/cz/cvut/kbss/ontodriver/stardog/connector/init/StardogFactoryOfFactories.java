package cz.cvut.kbss.ontodriver.stardog.connector.init;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectorFactoryImpl;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.rdf4j.connector.init.FactoryOfFactories;
import cz.cvut.kbss.ontodriver.rdf4j.connector.init.RepositoryConnectorInitializer;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.loader.DefaultContextInferenceStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.DefaultStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.StatementLoaderFactory;

public class StardogFactoryOfFactories implements FactoryOfFactories {

    private final DriverConfiguration config;
    private final RepositoryConnectorInitializer connectorInitializer;

    public StardogFactoryOfFactories(DriverConfiguration config) throws Rdf4jDriverException {
        this.config = config;
        this.connectorInitializer = new StardogRepositoryConnectionInitializer(config);
        connectorInitializer.initializeRepository();
    }

    @Override
    public ConnectorFactory createConnectorFactory() {
        final StorageConnector connector = new StorageConnector(connectorInitializer);
        return new ConnectorFactoryImpl(connector);
    }

    @Override
    public StatementLoaderFactory createStatementLoaderFactory() {
        if (config.is(Rdf4jConfigParam.INFERENCE_IN_DEFAULT_CONTEXT)) {
            return new DefaultContextInferenceStatementLoaderFactory();
        }
        return new DefaultStatementLoaderFactory();
    }
}
