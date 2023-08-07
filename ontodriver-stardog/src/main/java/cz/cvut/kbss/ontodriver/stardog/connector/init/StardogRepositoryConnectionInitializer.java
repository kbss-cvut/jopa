package cz.cvut.kbss.ontodriver.stardog.connector.init;

import com.complexible.stardog.api.ConnectionConfiguration;
import com.complexible.stardog.rdf4j.StardogRepository;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.connector.init.RepositoryConnectorInitializer;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.stardog.exception.StardogDriverException;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.manager.RepositoryManager;

public class StardogRepositoryConnectionInitializer extends RepositoryConnectorInitializer {

    private StardogRepository repository;

    private boolean connected;

    public StardogRepositoryConnectionInitializer(DriverConfiguration configuration) throws Rdf4jDriverException {
        super(configuration);
    }

    @Override
    public void initializeRepository() throws Rdf4jDriverException {
        final String url = configuration.getStorageProperties().getPhysicalURI().toString();
        try {
            final String serverUrl = url.substring(0, url.lastIndexOf('/') + 1);
            final String dbName = url.substring(serverUrl.length());
            final ConnectionConfiguration connConfig = ConnectionConfiguration.to(dbName).server(serverUrl);
            if (configuration.getStorageProperties().getUsername() != null) {
                connConfig.credentials(configuration.getStorageProperties()
                                                    .getUsername(), configuration.getStorageProperties().getPassword());
            }
            this.repository = new StardogRepository(connConfig);
            repository.init();
            this.connected = true;
        } catch (StringIndexOutOfBoundsException e) {
            throw new StardogDriverException("Invalid repository url '" + url + "'.");
        }
    }

    @Override
    public RepositoryManager getManager() {
        ensureState();
        return null;
    }

    private void ensureState() {
        if (!connected) {
            throw new IllegalStateException("Repository connection has not been initialized, yet!");
        }
    }

    @Override
    public Repository getRepository() {
        ensureState();
        return repository;
    }
}
