package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.owlapi.exceptions.OwlapiDriverException;
import cz.cvut.kbss.ontodriver_new.Connection;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Created by ledvima1 on 26.2.15.
 */
class OwlapiDriver implements Closeable, ConnectionListener {

    private final OntologyStorageProperties storageProperties;
    private final Map<String, String> properties;
    private boolean open = true;

    private final Set<OwlapiConnection> openConnections = new HashSet<>();

    OwlapiDriver(OntologyStorageProperties storageProperties, Map<String, String> properties) {
        this.storageProperties = storageProperties;
        this.properties = properties;
    }

    @Override
    public void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        for (OwlapiConnection c : openConnections) {
            try {
                c.removeListener(this);
                c.close();
            } catch (Exception e) {
                if (e instanceof OntoDriverException) {
                    throw (OntoDriverException) e;
                } else {
                    throw new OwlapiDriverException(e);
                }
            }
        }
        ConnectorFactory.getInstance(storageProperties, properties).close();
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    Connection acquireConnection() throws OntoDriverException {
        assert open;
        final OwlapiAdapter adapter = new OwlapiAdapter(
                ConnectorFactory.getInstance(storageProperties, properties).getConnector(), properties);
        final OwlapiConnection c = new OwlapiConnection(adapter);
        openConnections.add(c);
        c.addListener(this);
        return c;
    }

    @Override
    public void connectionClosed(Connection connection) {
        openConnections.remove(connection);
    }
}
