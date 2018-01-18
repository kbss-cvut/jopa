package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.jena.JenaDataSource;

import java.net.URI;

class StorageTestBase {

    static Configuration createConfiguration(String physicalUri) {
        return new Configuration(OntologyStorageProperties.driver(JenaDataSource.class.toString()).physicalUri(
                URI.create(physicalUri)).build());
    }
}
