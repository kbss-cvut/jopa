package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

import java.io.File;
import java.net.URI;

/**
 * Persistent storage configuration for Jena accessed single-file storage.
 *
 * @author ledvima1
 */
public class JenaStorageConfig extends StorageConfig {

    protected static final OntologyConnectorType TYPE = OntologyConnectorType.JENA;

    public JenaStorageConfig() {
        super();
    }

    @Override
    public OntologyStorageProperties createStorageProperties(int index) {
        assert index >= 0;
        assert name != null;
        assert directory != null;

        String base = name + TYPE.toString() + index;
        final URI ontoUri = URI.create(TestEnvironment.IRI_BASE + base);
        final File url = new File(directory + File.separator + base + ".owl");
        TestEnvironment.removeOldTestFiles(url);
        final URI physicalUri = url.toURI();

        return OntologyStorageProperties.ontologyUri(ontoUri).physicalUri(physicalUri).driver(TYPE.getDriverClass())
                                        .build();
    }

}
