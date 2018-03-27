package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.exception.ListProcessingException;
import cz.cvut.kbss.ontodriver.model.*;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.net.URI;
import java.util.*;

import static org.apache.jena.rdf.model.ResourceFactory.createStatement;

class SimpleListIterator {

    private final StorageConnector connector;

    private final Property hasList;
    private final Property hasNext;

    private final String context;

    private int index;
    private boolean removed = false;
    private Resource previous;
    private Resource current;
    private Collection<Statement> cursor;

    SimpleListIterator(Resource owner, Property hasList, Property hasNext, String context, StorageConnector connector) {
        this.current = owner;
        this.hasList = hasList;
        this.hasNext = hasNext;
        this.context = context;
        this.connector = connector;
        this.index = -1;
        moveCursor(current);
    }

    private void moveCursor(Resource from) {
        if (context != null) {
            this.cursor = connector.find(from, first() ? hasList : hasNext, null, context);
        } else {
            this.cursor = connector.find(from, first() ? hasList : hasNext, null);
        }
    }

    boolean hasNext() {
        return !cursor.isEmpty();
    }

    Axiom<NamedResource> next() {
        final NamedResource subject = NamedResource.create(current.getURI());
        final Assertion assertion = Assertion
                .createObjectPropertyAssertion(URI.create(first() ? hasList.getURI() : hasNext.getURI()), false);
        final NamedResource value = nextValue();
        return new AxiomImpl<>(subject, assertion, new Value<>(value));
    }

    private boolean first() {
        return index == -1;
    }

    NamedResource nextValue() {
        verifySuccessorCount();
        final RDFNode node = cursor.iterator().next().getObject();
        if (!node.isURIResource()) {
            throw new ListProcessingException("Expected successor of node " + current + " to be a named resource.");
        }
        final Resource item = node.asResource();
        index++;
        this.removed = false;
        this.previous = current;
        this.current = item;
        final NamedResource value = NamedResource.create(item.getURI());
        moveCursor(current);
        return value;
    }

    private void verifySuccessorCount() {
        if (!hasNext()) {
            throw new NoSuchElementException("No more elements available.");
        }
        if (cursor.size() > 1) {
            throw new IntegrityConstraintViolatedException(
                    "Encountered multiple successors of list node " + current.getURI());
        }
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
        assert previous != null;
        assert current != null;
        remove(previous, index == 0 ? hasList : hasNext, current);
        this.removed = true;
    }

    private void remove(Resource subject, Property property, RDFNode object) {
        if (context != null) {
            connector.remove(subject, property, object, context);
        } else {
            connector.remove(subject, property, object);
        }
    }

    /**
     * Replaces the current node with the specified replacement, connecting it into the list. Original links to the current
     * element are removed.
     *
     * @param replacement The replacement node
     */
    void replace(Resource replacement) {
        removeWithoutReconnect();
        final List<Statement> toAdd = new ArrayList<>(2);
        toAdd.add(createStatement(previous, index == 0 ? hasList : hasNext, replacement));
        if (hasNext()) {
            verifySuccessorCount();
            final RDFNode nextNode = cursor.iterator().next().getObject();
            remove(current, hasNext, nextNode);
            if (!nextNode.equals(replacement)) {
                final Statement linkToNext = createStatement(replacement, hasNext, nextNode);
                toAdd.add(linkToNext);
                this.cursor = Collections.singleton(linkToNext);
            } else {
                moveCursor(replacement);
            }
        }
        this.current = replacement;
        if (context != null) {
            connector.add(toAdd, context);
        } else {
            connector.add(toAdd);
        }
    }
}
