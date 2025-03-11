package cz.cvut.kbss.ontodriver.rdf4j.connector.init;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactory;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.loader.StatementLoaderFactory;

/**
 * Builds factories for the driver.
 */
public interface FactoryOfFactories {

    /**
     * Creates a {@link ConnectionFactory} instance.
     *
     * @return New connection factory
     * @throws Rdf4jDriverException When unable to init the factory (for example, unable to connect to the repository)
     */
    ConnectionFactory createConnectorFactory() throws OntoDriverException;

    /**
     * Creates a {@link StatementLoaderFactory} instance.
     *
     * @return New statement loader factory
     */
    StatementLoaderFactory createStatementLoaderFactory();
}
