package cz.cvut.kbss.ontodriver.virtuoso;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactory;
import cz.cvut.kbss.ontodriver.rdf4j.connector.init.FactoryOfFactories;
import cz.cvut.kbss.ontodriver.rdf4j.loader.DefaultStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.StatementLoaderFactory;
import cz.cvut.kbss.ontodriver.virtuoso.connector.VirtuosoConnectionFactory;
import org.eclipse.rdf4j.common.transaction.IsolationLevel;
import org.eclipse.rdf4j.common.transaction.IsolationLevels;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Optional;
import java.util.stream.Stream;

class VirtuosoFactoryOfFactories implements FactoryOfFactories {

    private static final Logger LOG = LoggerFactory.getLogger(VirtuosoFactoryOfFactories.class);

    private final DriverConfiguration configuration;

    public VirtuosoFactoryOfFactories(DriverConfiguration configuration) {
        this.configuration = configuration;
    }

    @Override
    public ConnectionFactory createConnectorFactory() throws VirtuosoDriverException {
        return new VirtuosoConnectionFactory(configuration, getTxIsolationLevel(configuration));
    }

    private static IsolationLevel getTxIsolationLevel(
            DriverConfiguration configuration) throws VirtuosoDriverException {
        final String isolationLevelConfig = configuration.getProperty(Rdf4jConfigParam.TRANSACTION_ISOLATION_LEVEL);
        if (isolationLevelConfig != null) {
            final Optional<IsolationLevels> optionalLevel = Stream.of(IsolationLevels.values())
                                                                  .filter(level -> level.toString()
                                                                                        .equals(isolationLevelConfig))
                                                                  .findAny();
            if (optionalLevel.isEmpty()) {
                throw new VirtuosoDriverException("Unsupported transaction isolation level value '" + isolationLevelConfig + "'.");
            }
            LOG.debug("Configured to use RDF4J transaction isolation level '{}'.", optionalLevel.get());
            return optionalLevel.get();
        }
        return null;
    }

    @Override
    public StatementLoaderFactory createStatementLoaderFactory() {
        return new DefaultStatementLoaderFactory();
    }
}
