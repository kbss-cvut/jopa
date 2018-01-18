package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.apache.jena.query.Dataset;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

abstract class Storage {

    static final Logger LOG = LoggerFactory.getLogger(Storage.class);

    final Configuration configuration;

    Dataset dataset;

    Storage(Configuration configuration) {
        this.configuration = configuration;
    }

    void writeChanges() throws OntoDriverException {
        // Do nothing by default
    }

    abstract void initialize();

    abstract Dataset getDataset();

    void close() {
        // Do nothing by default
    }
}
