/**
 * Copyright (C) 2019 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi.list;


import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableRemoveAxiom;
import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.*;
import java.util.stream.Collectors;

class SimpleListIterator extends OwlapiListIterator {

    final OWLObjectProperty hasNextProperty;

    OWLObjectProperty previousProperty;
    OWLObjectProperty currentProperty;
    OWLNamedIndividual previousNode;
    OWLNamedIndividual currentNode;
    Collection<? extends OWLIndividual> nextItem;

    private final OWLOntology ontology;
    private final OWLDataFactory dataFactory;

    final AxiomAdapter axiomAdapter;


    SimpleListIterator(ListDescriptor descriptor, OntologySnapshot snapshot, AxiomAdapter axiomAdapter) {
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
        return EntitySearcher.getObjectPropertyValues(currentNode, currentProperty, ontology).anyMatch(v -> true);
    }

    void doStep() {
        this.nextItem = EntitySearcher.getObjectPropertyValues(currentNode, currentProperty, ontology)
                                      .collect(Collectors.toSet());
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
        if (nextItem.isEmpty()) {
            throw new NoSuchElementException("There are no more elements.");
        }
        checkMaxSuccessors(previousProperty, nextItem);
        final OWLIndividual item = nextItem.iterator().next();
        checkIsNamed(item);
        this.currentNode = item.asOWLNamedIndividual();
        return getCurrentNode();
    }

    @Override
    NamedResource getCurrentNode() {
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

    /**
     * {@inheritDoc}
     * <p>
     * This method also replaces the current node with the new value, so subsequent calls to {@link #getCurrentNode()}
     * return the new value.
     *
     * @param newValue The new value to use
     * @return The changes to apply
     */
    @Override
    List<OWLOntologyChange> replaceNode(NamedResource newValue) {
        final List<OWLOntologyChange> changes = new ArrayList<>(4);
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
        final Iterator<OWLIndividual> nextOnes =
                EntitySearcher.getObjectPropertyValues(currentNode, currentProperty, ontology).iterator();
        return nextOnes.hasNext() ? nextOnes.next() : null;
    }
}
