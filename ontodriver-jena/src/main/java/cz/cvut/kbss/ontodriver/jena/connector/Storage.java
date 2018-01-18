package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.exception.OntoDriverInitializationException;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import org.apache.jena.query.Dataset;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

abstract class Storage {

    static final Logger LOG = LoggerFactory.getLogger(Storage.class);

    Dataset dataset;

    void writeChanges() throws OntoDriverException {
        // Do nothing by default
    }

    abstract void initialize();

    Dataset getDataset() {
        return dataset;
    }

    void close() {
        dataset.close();
    }

    /**
     * Creates a storage accessor according to the specified configuration.
     *
     * @param configuration Access configuration
     * @return Storage accessor instance
     * @throws OntoDriverInitializationException When storage type is not supported
     */
    static Storage create(Configuration configuration) {
        final String type = configuration.getProperty(JenaConfigParam.STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        final Storage storage;
        switch (type) {
            case JenaOntoDriverProperties.IN_MEMORY:
                storage = new MemoryStorage();
                break;
            case JenaOntoDriverProperties.FILE:
                storage = new FileStorage(configuration);
                break;
            case JenaOntoDriverProperties.SDB:
            case JenaOntoDriverProperties.TDB:
                throw new UnsupportedOperationException("Not implemented yet.");
            default:
                throw new OntoDriverInitializationException("Unsupported storage type \'" + type + "\'.");
        }
        storage.initialize();
        return storage;
    }
}
