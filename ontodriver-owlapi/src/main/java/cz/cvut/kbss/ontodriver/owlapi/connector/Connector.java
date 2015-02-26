package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exceptions.OwlapiDriverException;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.util.List;
import java.util.Map;

/**
 * Common superclass for the storage connectors.
 * <p/>
 * Declares the basic interface and stores storage info.
 *
 * @author ledvima1
 */
public abstract class Connector implements Closeable {

    protected OntologyStorageProperties storageProperties;
    protected Map<String, String> properties;

    private volatile boolean open;

    public Connector(OntologyStorageProperties storageProperties, Map<String, String> properties) throws
            OwlapiDriverException {
        assert storageProperties != null;
        assert properties != null;

        this.storageProperties = storageProperties;
        this.properties = properties;
        initializeConnector();
        this.open = true;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void close() throws OntoDriverException {
        this.open = false;
    }

    protected void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The connector is closed.");
        }
    }

    /**
     * Initializes the connector.
     */
    protected abstract void initializeConnector() throws OwlapiDriverException;

    /**
     * Gets snapshot of the underlying ontology.
     *
     * @return Value object with the ontology snapshot
     */
    public abstract OntologyStructures getOntologySnapshot();

    /**
     * Applies the specified changes to the underlying ontology.
     * <p/>
     * Note that this operation is atomic - the changes are applied as a whole and no other operation can be performed
     * on the underlying ontology while the changes are being applied.
     *
     * @param changes The changes to apply
     */
    public abstract void applyChanges(List<OWLOntologyChange> changes);
}
