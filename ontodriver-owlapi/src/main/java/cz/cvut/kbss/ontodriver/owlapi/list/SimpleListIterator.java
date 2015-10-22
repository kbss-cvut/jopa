package cz.cvut.kbss.ontodriver.owlapi.list;


import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.Collection;
import java.util.NoSuchElementException;

class SimpleListIterator extends OwlapiListIterator {

    final OWLObjectProperty hasNextProperty;

    OWLObjectProperty previousProperty;
    OWLObjectProperty currentProperty;
    OWLNamedIndividual previousNode;
    OWLNamedIndividual currentNode;
    Collection<? extends OWLIndividual> next;

    private final OWLOntology ontology;

    private final AxiomAdapter axiomAdapter;


    SimpleListIterator(SimpleListDescriptor descriptor, OntologyStructures snapshot, AxiomAdapter axiomAdapter) {
        this.ontology = snapshot.getOntology();
        final OWLDataFactory dataFactory = snapshot.getDataFactory();
        this.previousProperty = dataFactory
                .getOWLObjectProperty(IRI.create(descriptor.getListProperty().getIdentifier()));
        this.hasNextProperty = dataFactory.getOWLObjectProperty(IRI.create(descriptor.getNextNode().getIdentifier()));
        this.currentNode = dataFactory.getOWLNamedIndividual(IRI.create(descriptor.getListOwner().getIdentifier()));
        this.currentProperty = previousProperty;
        this.axiomAdapter = axiomAdapter;
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
            throw new NoSuchElementException("There are no more elements.");
        }
        checkMaxSuccessors(previousProperty, next);
        final OWLIndividual item = next.iterator().next();
        checkIsNamed(item);
        this.currentNode = item.asOWLNamedIndividual();
        final NamedResource subject = NamedResource.create(previousNode.getIRI().toURI());
        final NamedResource value = NamedResource.create(currentNode.getIRI().toURI());
        final Assertion assertion = Assertion.createObjectPropertyAssertion(previousProperty.getIRI().toURI(), false);
        final Axiom<NamedResource> ax = axiomAdapter.createAxiom(subject, assertion, value);
        doStep();
        return ax;
    }

    void doStep() {
        this.next = EntitySearcher.getObjectPropertyValues(currentNode, currentProperty, ontology);
        this.previousProperty = currentProperty;
        this.currentProperty = hasNextProperty;
        this.previousNode = this.currentNode;
    }
}
