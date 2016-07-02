package cz.cvut.kbss.ontodriver.sesame.environment;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.ConfigParam;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.SesameDataSource;
import cz.cvut.kbss.ontodriver.sesame.config.SesameConfigParam;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

import java.net.URI;

public class TestRepositoryProvider {

    private final ConnectorFactory factory = ConnectorFactory.getInstance();

    public Connector createConnector(boolean useInference) throws SesameDriverException {
        OntologyStorageProperties storageProperties = OntologyStorageProperties
                .physicalUri(URI.create("TestStore"))
                .driver(SesameDataSource.class.getCanonicalName())
                .build();
        final Configuration configuration = new Configuration(storageProperties);

        configuration.setProperty(SesameConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        configuration.setProperty(SesameConfigParam.USE_INFERENCE, Boolean.toString(useInference));
        configuration.setProperty(ConfigParam.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        configuration.setProperty(ConfigParam.ONTOLOGY_LANGUAGE, "en");
        return factory.createStorageConnector(configuration);
    }

    public void close() throws OntoDriverException {
        factory.close();
    }
}
