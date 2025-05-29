/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;

class InferredReferencedListIterator<T> extends ReferencedListIterator<T> {

    private OWLNamedIndividual currentNode;
    private final OWLReasoner reasoner;

    InferredReferencedListIterator(ReferencedListDescriptor descriptor, OntologySnapshot snapshot,
                                   AxiomAdapter axiomAdapter) {
        super(descriptor, snapshot, axiomAdapter);
        this.reasoner = snapshot.reasoner();
        if (reasoner == null) {
            throw new ReasonerNotAvailableException();
        }
        this.currentNode = OwlapiUtils.getIndividual(descriptor.getListOwner(), snapshot.dataFactory());
    }

    @Override
    public boolean hasNext() {
        if (nextItem == null) {
            doStep();
        }
        return nextItem != null && !nextItem.isEmpty();
    }

    @Override
    void doStep() {
        final Collection<OWLNamedIndividual> nextNodes =
                reasoner.getObjectPropertyValues(currentNode, currentNextNodeProperty).entities()
                        .collect(Collectors.toSet());
        if (nextNodes.isEmpty()) {
            this.nextItem = Collections.emptyList();
            return;
        }
        checkMaxSuccessors(currentNextNodeProperty, nextNodes);
        this.currentNextNodeProperty = hasNextProperty;
        this.currentNode = nextNodes.iterator().next();
        this.nextItem = hasContentProperty.isOWLObjectProperty() ? reasoner.getObjectPropertyValues(currentNode, hasContentProperty.asOWLObjectProperty())
                                                                           .entities()
                                                                           .collect(Collectors.toSet())
                : reasoner.getDataPropertyValues(currentNode, hasContentProperty.asOWLDataProperty());
    }

    @Override
    T nextValue() {
        if (!hasNext()) {
            throw new NoSuchElementException("There are no more elements.");
        }
        final T result = extractNodeContent();
        doStep();
        return result;
    }

    @Override
    List<TransactionalChange> removeWithoutReconnect() {
        throw new UnsupportedOperationException();
    }

    @Override
    List<TransactionalChange> replaceNode(T newValue) {
        throw new UnsupportedOperationException();
    }
}
