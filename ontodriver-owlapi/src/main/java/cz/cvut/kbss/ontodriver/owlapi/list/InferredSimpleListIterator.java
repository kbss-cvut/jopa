package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.Collections;

class InferredSimpleListIterator extends SimpleListIterator {

    private final OWLReasoner reasoner;

    InferredSimpleListIterator(SimpleListDescriptor descriptor,
                               OntologyStructures snapshot,
                               AxiomAdapter axiomAdapter) {
        super(descriptor, snapshot, axiomAdapter);
        if (snapshot.getReasoner() == null) {
            throw new ReasonerNotAvailableException();
        }
        this.reasoner = snapshot.getReasoner();
    }

    @Override
    void doStep() {
        final NodeSet<OWLNamedIndividual> nodeSet = reasoner.getObjectPropertyValues(currentNode, currentProperty);
        this.next = nodeSet.isEmpty() ? Collections.emptySet() : nodeSet.getFlattened();
        this.previousProperty = currentProperty;
        this.currentProperty = hasNextProperty;
        this.previousNode = this.currentNode;
    }
}
