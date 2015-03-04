package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by ledvima1 on 26.2.15.
 */
class OwlapiAdapter {

    private final Connector connector;
    private OntologyStructures ontologySnapshot;

    private TransactionState transactionState = TransactionState.INITIAL;
    private List<OWLOntologyChange> pendingChanges = new ArrayList<>();

    private static enum TransactionState {
        INITIAL, RUNNING
    }

    public OwlapiAdapter(Connector connector) {
        this.connector = connector;
    }

    private void startTransactionIfNotActive() {
        if (transactionState == TransactionState.INITIAL) {
            this.ontologySnapshot = connector.getOntologySnapshot();
            this.transactionState = TransactionState.RUNNING;
        }
    }

    void commit() {
        if (transactionState != TransactionState.RUNNING) {
            return;
        }
        if (!pendingChanges.isEmpty()) {
            connector.applyChanges(pendingChanges);
            this.pendingChanges = new ArrayList<>();
        }
        transactionCleanup();
    }

    private void transactionCleanup() {
        this.ontologySnapshot = null;
        this.transactionState = TransactionState.INITIAL;
    }

    void rollback() {
        if (transactionState != TransactionState.RUNNING) {
            return;
        }
        if (!pendingChanges.isEmpty()) {
            pendingChanges = new ArrayList<>();
        }
        transactionCleanup();
    }

    boolean isConsistent(URI context) {
        final IRI ontoIri = ontologySnapshot.getOntology().getOntologyID().getOntologyIRI().get();
        if (!IRI.create(context).equals(ontoIri)) {
            throw new IllegalArgumentException("Invalid URI passed to isConsistent. Expected " + ontoIri + ", but got " + context);
        }
        return ontologySnapshot.getReasoner().isConsistent();
    }
}
