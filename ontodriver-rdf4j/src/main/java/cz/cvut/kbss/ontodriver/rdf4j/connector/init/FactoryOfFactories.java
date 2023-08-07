package cz.cvut.kbss.ontodriver.rdf4j.connector.init;

import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.loader.StatementLoaderFactory;

/**
 * Creates factories for use by the driver.
 */
public interface FactoryOfFactories {

    /**
     * creates a {@link ConnectorFactory} instance.
     *
     * @return ConnectorFactory instance
     * @throws Rdf4jDriverException If unable to connect to the underlying repository
     */
    ConnectorFactory createConnectorFactory() throws Rdf4jDriverException;

    /**
     * Creates a factory for statement loading.
     *
     * @return StatementLoaderFactory instance
     * @throws Rdf4jDriverException If unable to create the factory
     */
    StatementLoaderFactory createStatementLoaderFactory() throws Rdf4jDriverException;
}
