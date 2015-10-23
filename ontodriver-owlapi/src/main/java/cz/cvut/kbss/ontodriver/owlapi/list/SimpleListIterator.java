package cz.cvut.kbss.ontodriver.owlapi.list;


import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableRemoveAxiom;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.*;

class SimpleListIterator extends OwlapiListIterator {

    final OWLObjectProperty hasNextProperty;

    OWLObjectProperty previousProperty;
    OWLObjectProperty currentProperty;
    OWLNamedIndividual previousNode;
    OWLNamedIndividual currentNode;
    Collection<? extends OWLIndividual> next;

    private final OWLOntology ontology;
    private final OWLDataFactory dataFactory;

    final AxiomAdapter axiomAdapter;


    SimpleListIterator(SimpleListDescriptor descriptor, OntologyStructures snapshot, AxiomAdapter axiomAdapter) {
        this.ontology = snapshot.getOntology();
        this.dataFactory = snapshot.getDataFactory();
        this.previousProperty = dataFactory
                .getOWLObjectProperty(IRI.create(descriptor.getListProperty().getIdentifier()));
        this.hasNextProperty = dataFactory.getOWLObjectProperty(IRI.create(descriptor.getNextNode().getIdentifier()));
        this.currentNode = dataFactory.getOWLNamedIndividual(IRI.create(descriptor.getListOwner().getIdentifier()));
        this.currentProperty = previousProperty;
        this.axiomAdapter = axiomAdapter;
    }

    @Override
    public boolean hasNext() {
        return !EntitySearcher.getObjectPropertyValues(currentNode, currentProperty, ontology).isEmpty();
    }

    void doStep() {
        this.next = EntitySearcher.getObjectPropertyValues(currentNode, currentProperty, ontology);
        this.previousProperty = currentProperty;
        this.currentProperty = hasNextProperty;
        this.previousNode = this.currentNode;
    }

    @Override
    public Axiom<NamedResource> next() {
        final NamedResource value = nextValue();
        final NamedResource subject = NamedResource.create(previousNode.getIRI().toURI());
        final Assertion assertion = Assertion.createObjectPropertyAssertion(previousProperty.getIRI().toURI(), false);
        return axiomAdapter.createAxiom(subject, assertion, value);
    }

    @Override
    NamedResource nextValue() {
        doStep();
        if (next.isEmpty()) {
            throw new NoSuchElementException("There are no more elements.");
        }
        checkMaxSuccessors(previousProperty, next);
        final OWLIndividual item = next.iterator().next();
        checkIsNamed(item);
        this.currentNode = item.asOWLNamedIndividual();
        return NamedResource.create(currentNode.getIRI().toURI());
    }

    @Override
    List<OWLOntologyChange> removeWithoutReconnect() {
        final List<OWLOntologyChange> changes = new ArrayList<>(2);
        changes.add(new MutableRemoveAxiom(ontology,
                dataFactory.getOWLObjectPropertyAssertionAxiom(previousProperty, previousNode, currentNode)));
        final OWLIndividual nextNode = getNextNode();
        if (nextNode != null) {
            changes.add(new MutableRemoveAxiom(ontology,
                    dataFactory.getOWLObjectPropertyAssertionAxiom(currentProperty, currentNode, nextNode)));
        }
        return changes;
    }

    @Override
    List<OWLOntologyChange> replaceNode(NamedResource newValue) {
        final List<OWLOntologyChange> changes = new ArrayList<>(2);
        changes.addAll(removeWithoutReconnect());
        final OWLNamedIndividual newNode = dataFactory.getOWLNamedIndividual(IRI.create(newValue.getIdentifier()));
        changes.add(new MutableAddAxiom(ontology,
                dataFactory.getOWLObjectPropertyAssertionAxiom(previousProperty, previousNode, newNode)));
        final OWLOntologyChange connectionToNext = connectToNextNode(newNode);
        if (connectionToNext != null) {
            changes.add(connectionToNext);
        }
        this.currentNode = newNode;
        return changes;
    }

    private OWLOntologyChange connectToNextNode(OWLNamedIndividual newNode) {
        final OWLIndividual nextNode = getNextNode();
        if (nextNode == null || nextNode.equals(newNode)) {
            return null;
        }
        return new MutableAddAxiom(ontology,
                dataFactory.getOWLObjectPropertyAssertionAxiom(currentProperty, newNode, nextNode));
    }

    private OWLIndividual getNextNode() {
        final Collection<OWLIndividual> nextOnes = EntitySearcher
                .getObjectPropertyValues(currentNode, currentProperty, ontology);
        if (nextOnes.isEmpty()) {
            return null;
        }
        return nextOnes.iterator().next();
    }
}
