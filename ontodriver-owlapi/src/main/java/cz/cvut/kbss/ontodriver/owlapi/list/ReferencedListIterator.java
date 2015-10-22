package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.Collection;
import java.util.Collections;
import java.util.NoSuchElementException;

class ReferencedListIterator extends OwlapiListIterator {

    final OWLObjectProperty hasNextProperty;
    final OWLObjectProperty hasContentProperty;

    final OWLOntology ontology;

    final AxiomAdapter axiomAdapter;

    OWLIndividual currentNode;
    OWLObjectProperty currentNextNodeProperty;
    Collection<? extends OWLIndividual> next;

    final ReferencedListDescriptor descriptor;

    ReferencedListIterator(ReferencedListDescriptor descriptor, OntologyStructures snapshot,
                           AxiomAdapter axiomAdapter) {
        this.ontology = snapshot.getOntology();
        final OWLDataFactory dataFactory = snapshot.getDataFactory();
        this.hasNextProperty = dataFactory.getOWLObjectProperty(IRI.create(descriptor.getNextNode().getIdentifier()));
        this.hasContentProperty = dataFactory
                .getOWLObjectProperty(IRI.create(descriptor.getNodeContent().getIdentifier()));
        this.axiomAdapter = axiomAdapter;
        this.currentNextNodeProperty = dataFactory
                .getOWLObjectProperty(IRI.create(descriptor.getListProperty().getIdentifier()));
        this.currentNode = OwlapiUtils.getIndividual(descriptor.getListOwner(), dataFactory);
        this.descriptor = descriptor;
    }

    @Override
    public boolean hasNext() {
        if (next == null) {
            doStep();
        }
        return !next.isEmpty();
    }

    @Override
    public Axiom<NamedResource> next() {
        if (!hasNext()) {
            throw new NoSuchElementException("No more elements in this referenced list.");
        }
        checkMaxSuccessors(hasContentProperty, next);
        final OWLIndividual value = next.iterator().next();
        checkIsNamed(value);
        final Axiom<NamedResource> axiom = axiomAdapter.createAxiom(
                NamedResource.create(currentNode.asOWLNamedIndividual().getIRI().toURI()),
                descriptor.getNodeContent(), NamedResource.create(value.asOWLNamedIndividual().getIRI().toURI()));
        doStep();
        return axiom;
    }

    void doStep() {
        final Collection<OWLIndividual> nextNodes = EntitySearcher
                .getObjectPropertyValues(currentNode, currentNextNodeProperty, ontology);
        if (nextNodes.isEmpty()) {
            this.next = Collections.emptyList();
            return;
        }
        checkMaxSuccessors(currentNextNodeProperty, nextNodes);
        this.currentNextNodeProperty = hasNextProperty; // This just switches from hasList to hasNext
        final OWLIndividual node = nextNodes.iterator().next();
        checkIsNamed(node);
        this.currentNode = node;
        this.next = EntitySearcher.getObjectPropertyValues(node, hasContentProperty, ontology);
    }
}
