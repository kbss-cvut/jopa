/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;

/**
 * This class differs from the way basic SimpleListIterator iterates over the list. Here, we do step before even calling
 * nextItem in order to avoid consulting the reasoner twice for values of the current node's successors.
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
        if (nextItem == null) {
            doStep();
        }
        return !nextItem.isEmpty();
    }

    @Override
    void doStep() {
        final NodeSet<OWLNamedIndividual> nodeSet = reasoner.getObjectPropertyValues(currentNode, currentProperty);
        this.nextItem = nodeSet.isEmpty() ? Collections.emptySet() : nodeSet.entities().collect(Collectors.toSet());
        this.previousProperty = currentProperty;
        this.currentProperty = hasNextProperty;
        this.previousNode = this.currentNode;
    }

    @Override
    public Axiom<NamedResource> next() {
        if (!hasNext()) {
            throw new NoSuchElementException("There are no more elements.");
        }
        checkMaxSuccessors(previousProperty, nextItem);
        final OWLIndividual item = nextItem.iterator().next();  // We know the individual is named
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
