package cz.cvut.kbss.ontodriver.rdf4j.connector.init;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.config.Constants;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jOntoDriverProperties;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.manager.RepositoryManager;

/**
 * Initializes connection to a RDF4J-compatible repository.
 */
public abstract class RepositoryConnectorInitializer {

    protected final DriverConfiguration configuration;

    private final int maxReconnectAttempts;

    protected RepositoryConnectorInitializer(DriverConfiguration configuration) throws Rdf4jDriverException {
        this.configuration = configuration;
        this.maxReconnectAttempts = resolveMaxReconnectAttempts();
    }

    private int resolveMaxReconnectAttempts() throws Rdf4jDriverException {
        try {
            final int attempts = configuration.isSet(Rdf4jConfigParam.RECONNECT_ATTEMPTS) ? Integer.parseInt(
                    configuration.getProperty(Rdf4jConfigParam.RECONNECT_ATTEMPTS)) :
                    Constants.DEFAULT_RECONNECT_ATTEMPTS_COUNT;
            if (attempts < 0) {
                throw invalidReconnectAttemptsConfig();
            }
            return attempts;
        } catch (NumberFormatException e) {
            throw invalidReconnectAttemptsConfig();
        }
    }

    private static Rdf4jDriverException invalidReconnectAttemptsConfig() {
        return new Rdf4jDriverException(
                "Invalid value of configuration parameter " + Rdf4jOntoDriverProperties.RECONNECT_ATTEMPTS +
                        ". Must be a non-negative integer.");
    }


    /**
     * Initializes the repository connection.
     * <p>
     * This method must be called before any other methods related to repository access.
     *
     * @throws Rdf4jDriverException If unable to connect to the repository
     */
    public abstract void initializeRepository() throws Rdf4jDriverException;

    /**
     * Gets the {@link RepositoryManager} of the repository to which this instance is connected.
     *
     * @return Connected repository manager
     * @throws IllegalStateException If the connection has not been made, yet
     */
    public abstract RepositoryManager getManager();

    /**
     * Gets the {@link Repository} to which this connector is connected.
     *
     * @return Connected RDF4J repository
     * @throws IllegalStateException If the connection has not been made, yet
     */
    public abstract Repository getRepository();

    /**
     * Gets the configured maximum number of reconnection attempts.
     * <p>
     * This is the number of attempts the driver will make to reconnect to a repository in case the connection is lost.
     *
     * @return Maximum configured number of reconnection attempts
     */
    public int getMaxReconnectAttempts() {
        return maxReconnectAttempts;
    }
}
