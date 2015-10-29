package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
import cz.cvut.kbss.ontodriver_new.descriptors.ListDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * This class differs from the way basic SimpleListIterator iterates over the list. Here, we do step before even calling
 * next in order to avoid consulting the reasoner twice for values of the current node's successors.
 * <p>
 * We can do this because we need not worry about remove operations, they're not supported on inferred iterator.
 * <p>
 * TODO Check if the inferred iterators' logic is correct, especially for the nextValue method
 */
class InferredSimpleListIterator extends SimpleListIterator {

    private final OWLReasoner reasoner;

    InferredSimpleListIterator(ListDescriptor descriptor, OntologySnapshot snapshot,
                               AxiomAdapter axiomAdapter) {
        super(descriptor, snapshot, axiomAdapter);
        if (snapshot.getReasoner() == null) {
            throw new ReasonerNotAvailableException();
        }
        this.reasoner = snapshot.getReasoner();
    }

    @Override
    public boolean hasNext() {
        if (next == null) {
            doStep();
        }
        return !next.isEmpty();
    }

    @Override
    void doStep() {
        final NodeSet<OWLNamedIndividual> nodeSet = reasoner.getObjectPropertyValues(currentNode, currentProperty);
        this.next = nodeSet.isEmpty() ? Collections.emptySet() : nodeSet.getFlattened();
        this.previousProperty = currentProperty;
        this.currentProperty = hasNextProperty;
        this.previousNode = this.currentNode;
    }

    @Override
    public Axiom<NamedResource> next() {
        if (!hasNext()) {
            throw new NoSuchElementException("There are no more elements.");
        }
        checkMaxSuccessors(previousProperty, next);
        final OWLIndividual item = next.iterator().next();  // We know the individual is named
        this.currentNode = item.asOWLNamedIndividual();
        final NamedResource subject = NamedResource.create(previousNode.getIRI().toURI());
        final NamedResource value = NamedResource.create(currentNode.getIRI().toURI());
        final Assertion assertion = Assertion.createObjectPropertyAssertion(previousProperty.getIRI().toURI(), false);
        doStep();
        return axiomAdapter.createAxiom(subject, assertion, value);
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
