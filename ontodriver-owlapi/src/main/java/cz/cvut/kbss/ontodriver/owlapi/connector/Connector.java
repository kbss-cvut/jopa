package cz.cvut.kbss.ontodriver.owlapi.connector;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.util.List;

/**
 * Storage connector interface.
 * <p>
 * This interface declares the methods accessible from the driver.
 */
public interface Connector {

    /**
     * Gets snapshot of the underlying ontology.
     *
     * @return Value object with the ontology snapshot
     */
    OntologyStructures getOntologySnapshot();

    /**
     * Applies the specified changes to the underlying ontology.
     * <p>
     * Note that this operation is atomic - the changes are applied as a whole and no other operation can be performed
     * on the underlying ontology while the changes are being applied.
     *
     * @param changes The changes to apply
     */
    void applyChanges(List<OWLOntologyChange> changes);
}
