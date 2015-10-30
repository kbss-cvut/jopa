package cz.cvut.kbss.ontodriver.owlapi.connector;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * Storage connector interface.
 * <p>
 * This interface declares the methods accessible from the driver.
 */
public interface Connector {

    /**
     * Gets snapshot of the underlying ontology.
     * <p>
     * The snapshot is completely independent of the live ontology, so any changes to either are not visible to the
     * other.
     *
     * @return Value object with the ontology snapshot
     */
    OntologySnapshot getOntologySnapshot();

    <R> R executeRead(Function<OntologySnapshot, R> function);

    void executeWrite(Consumer<OntologySnapshot> function);

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
