package cz.cvut.kbss.ontodriver.virtuoso;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactory;
import cz.cvut.kbss.ontodriver.rdf4j.connector.init.FactoryOfFactories;
import cz.cvut.kbss.ontodriver.rdf4j.loader.DefaultStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.StatementLoaderFactory;

class VirtuosoFactoryOfFactories implements FactoryOfFactories {

    private final DriverConfiguration configuration;

    public VirtuosoFactoryOfFactories(DriverConfiguration configuration) {
        this.configuration = configuration;
    }

    @Override
    public ConnectionFactory createConnectorFactory() throws VirtuosoDriverException {
        return null;
    }

    @Override
    public StatementLoaderFactory createStatementLoaderFactory() {
        return new DefaultStatementLoaderFactory();
    }
}
