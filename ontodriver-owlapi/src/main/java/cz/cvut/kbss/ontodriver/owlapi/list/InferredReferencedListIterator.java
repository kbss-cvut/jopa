package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

class InferredReferencedListIterator extends ReferencedListIterator {

    private OWLNamedIndividual currentNode;
    private OWLReasoner reasoner;

    InferredReferencedListIterator(ReferencedListDescriptor descriptor, OntologyStructures snapshot,
                                   AxiomAdapter axiomAdapter) {
        super(descriptor, snapshot, axiomAdapter);
        this.reasoner = snapshot.getReasoner();
        if (reasoner == null) {
            throw new ReasonerNotAvailableException();
        }
        this.currentNode = OwlapiUtils.getIndividual(descriptor.getListOwner(), snapshot.getDataFactory());
    }

    @Override
    void doStep() {
        final Collection<OWLNamedIndividual> nextNodes = reasoner
                .getObjectPropertyValues(currentNode, currentNextNodeProperty).getFlattened();
        if (nextNodes.isEmpty()) {
            this.next = Collections.emptyList();
            return;
        }
        checkMaxSuccessors(currentNextNodeProperty, nextNodes);
        this.currentNextNodeProperty = hasNextProperty;
        this.currentNode = nextNodes.iterator().next();
        this.next = reasoner.getObjectPropertyValues(currentNode, hasContentProperty).getFlattened();
    }

    @Override
    List<OWLOntologyChange> removeWithoutReconnect() {
        throw new UnsupportedOperationException();
    }

    @Override
    List<OWLOntologyChange> replaceNode(NamedResource newValue) {
        throw new UnsupportedOperationException();
    }
}
