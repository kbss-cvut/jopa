package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.Connection;

import java.util.Map;

/**
 * Created by ledvima1 on 26.2.15.
 */
class OwlapiDriver implements Closeable {

    private final OntologyStorageProperties storageProperties;
    private final Map<String, String> properties;
    private boolean open = true;

    OwlapiDriver(OntologyStorageProperties storageProperties, Map<String, String> properties) {
        this.storageProperties = storageProperties;
        this.properties = properties;
    }

    @Override
    public void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    Connection acquireConnection() {
        // TODO
        return new OwlapiConnection();
    }
}
