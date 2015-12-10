package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableRemoveAxiom;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.*;

class ReferencedListIterator extends OwlapiListIterator {

    final OWLObjectProperty hasNextProperty;
    final OWLObjectProperty hasContentProperty;

    final OWLOntology ontology;
    private final OWLDataFactory dataFactory;

    final AxiomAdapter axiomAdapter;

    OWLIndividual previousNode;
    OWLIndividual currentNode;
    OWLObjectProperty previousNextNodeProperty;
    OWLObjectProperty currentNextNodeProperty;
    Collection<? extends OWLIndividual> next;

    final ReferencedListDescriptor descriptor;

    ReferencedListIterator(ReferencedListDescriptor descriptor, OntologySnapshot snapshot,
                           AxiomAdapter axiomAdapter) {
        this.ontology = snapshot.getOntology();
        this.dataFactory = snapshot.getDataFactory();
        this.hasNextProperty = dataFactory.getOWLObjectProperty(IRI.create(descriptor.getNextNode().getIdentifier()));
        this.hasContentProperty = dataFactory
                .getOWLObjectProperty(IRI.create(descriptor.getNodeContent().getIdentifier()));
        this.axiomAdapter = axiomAdapter;
        this.currentNextNodeProperty = dataFactory
                .getOWLObjectProperty(IRI.create(descriptor.getListProperty().getIdentifier()));
        this.previousNextNodeProperty = currentNextNodeProperty;
        this.currentNode = OwlapiUtils.getIndividual(descriptor.getListOwner(), dataFactory);
        this.previousNode = currentNode;
        this.descriptor = descriptor;
    }

    @Override
    public boolean hasNext() {
        return !EntitySearcher.getObjectPropertyValues(currentNode, currentNextNodeProperty, ontology).isEmpty();
    }

    void doStep() {
        final Collection<OWLIndividual> nextNodes = EntitySearcher
                .getObjectPropertyValues(currentNode, currentNextNodeProperty, ontology);
        if (nextNodes.isEmpty()) {
            this.next = Collections.emptyList();
            return;
        }
        checkMaxSuccessors(currentNextNodeProperty, nextNodes);
        this.previousNextNodeProperty = currentNextNodeProperty;
        this.currentNextNodeProperty = hasNextProperty; // This just switches from hasList to hasNext
        final OWLIndividual node = nextNodes.iterator().next();
        checkIsNamed(node);
        this.previousNode = currentNode;
        this.currentNode = node;
        this.next = EntitySearcher.getObjectPropertyValues(node, hasContentProperty, ontology);
    }

    @Override
    public Axiom<NamedResource> next() {
        final NamedResource value = nextValue();
        return axiomAdapter
                .createAxiom(NamedResource.create(currentNode.asOWLNamedIndividual().getIRI().toURI()),
                        descriptor.getNodeContent(), value);
    }

    @Override
    NamedResource nextValue() {
        doStep();
        if (next.isEmpty()) {
            throw new NoSuchElementException("There are no more elements.");
        }
        checkMaxSuccessors(hasContentProperty, next);
        final OWLIndividual value = next.iterator().next();
        checkIsNamed(value);
        return NamedResource.create(value.asOWLNamedIndividual().getIRI().toURI());
    }

    @Override
    public NamedResource getCurrentNode() {
        return NamedResource.create(currentNode.asOWLNamedIndividual().getIRI().toURI());
    }

    @Override
    List<OWLOntologyChange> removeWithoutReconnect() {
        final List<OWLOntologyChange> changes = new ArrayList<>(2);
        changes.add(new MutableRemoveAxiom(ontology,
                dataFactory.getOWLObjectPropertyAssertionAxiom(previousNextNodeProperty, previousNode, currentNode)));
        final OWLIndividual nextNode = getNextNode();
        if (nextNode != null) {
            changes.add(new MutableRemoveAxiom(ontology,
                    dataFactory.getOWLObjectPropertyAssertionAxiom(currentNextNodeProperty, currentNode, nextNode)));
        }
        return changes;
    }

    private OWLIndividual getNextNode() {
        final Collection<OWLIndividual> nextOnes = EntitySearcher
                .getObjectPropertyValues(currentNode, currentNextNodeProperty, ontology);
        if (nextOnes.isEmpty()) {
            return null;
        }
        return nextOnes.iterator().next();
    }

    @Override
    List<OWLOntologyChange> replaceNode(NamedResource newValue) {
        // We know there is exactly one, because next has to have been called before this method
        final OWLIndividual originalContent = next.iterator().next();
        final OWLNamedIndividual newContent = OwlapiUtils.getIndividual(newValue, dataFactory);
        final List<OWLOntologyChange> changes = new ArrayList<>(2);
        changes.add(new MutableRemoveAxiom(ontology,
                dataFactory.getOWLObjectPropertyAssertionAxiom(hasContentProperty, currentNode, originalContent)));
        changes.add(new MutableAddAxiom(ontology,
                dataFactory.getOWLObjectPropertyAssertionAxiom(hasContentProperty, currentNode, newContent)));
        return changes;
    }
}
