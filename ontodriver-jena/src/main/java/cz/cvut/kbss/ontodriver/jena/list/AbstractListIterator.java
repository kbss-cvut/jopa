/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.exception.ListProcessingException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;
import java.util.NoSuchElementException;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;

abstract class AbstractListIterator {

    final StorageConnector connector;

    final Property hasListProperty;
    final Property hasNextProperty;

    final String context;

    int index;
    private boolean removed = false;

    Resource previousNode;
    Resource currentNode;
    Collection<Statement> cursor;

    AbstractListIterator(ListDescriptor descriptor, StorageConnector connector) {
        this.hasListProperty = createProperty(descriptor.getListProperty().getIdentifier().toString());
        this.hasNextProperty = createProperty(descriptor.getNextNode().getIdentifier().toString());
        this.context = descriptor.getContext() != null ? descriptor.getContext().toString() : null;
        this.connector = connector;
        this.index = -1;
        this.currentNode = createResource(descriptor.getListOwner().getIdentifier().toString());
        moveCursor(currentNode);
    }

    void moveCursor(Resource from) {
        this.cursor = connector.find(from, first() ? hasListProperty : hasNextProperty, null, context);
    }

    void resolveNextListNode() {
        verifySuccessorCount();
        final RDFNode node = cursor.iterator().next().getObject();
        if (!node.isURIResource()) {
            throw new ListProcessingException("Expected successor of node " + currentNode + " to be a named resource.");
        }
        final Resource item = node.asResource();
        index++;
        this.removed = false;
        this.previousNode = currentNode;
        this.currentNode = item;
        moveCursor(currentNode);
    }

    boolean first() {
        return index == -1;
    }

    boolean hasNext() {
        return !cursor.isEmpty();
    }

    void verifySuccessorCount() {
        if (!hasNext()) {
            throw new NoSuchElementException("No more elements available.");
        }
        if (cursor.size() > 1) {
            throw new IntegrityConstraintViolatedException(
                    "Encountered multiple successors of list node " + currentNode.getURI());
        }
    }

    void remove(Resource subject, Property property, RDFNode object) {
        connector.remove(subject, property, object, context);
    }

    abstract Axiom<NamedResource> nextAxiom();

    abstract NamedResource nextValue();

    /**
     * Only returns current node.
     * <p>
     * Does not advance the iterator like {@link #nextAxiom()} and {@link #nextValue()} do.
     *
     * @return Current list node
     */
    Resource getCurrentNode() {
        return currentNode;
    }

    /**
     * Removes the current node without reconnecting the subsequent nodes to the previous one.
     */
    void removeWithoutReconnect() {
        if (first()) {
            throw new IllegalStateException("Cannot call remove before calling next.");
        }
        if (removed) {
            throw new IllegalStateException("Cannot call remove multiple times on one element.");
        }
        assert previousNode != null;
        assert currentNode != null;
        remove(previousNode, index == 0 ? hasListProperty : hasNextProperty, currentNode);
        this.removed = true;
    }

    abstract void replace(Resource replacement);
}
