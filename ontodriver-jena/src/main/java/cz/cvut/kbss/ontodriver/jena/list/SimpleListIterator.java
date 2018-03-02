package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.*;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.net.URI;
import java.util.Collection;
import java.util.NoSuchElementException;

class SimpleListIterator {

    private final StorageConnector connector;

    private final Property hasList;
    private final Property hasNext;

    private boolean first;
    private Resource previous;
    private Collection<Statement> cursor;

    SimpleListIterator(Resource owner, Property hasList, Property hasNext, StorageConnector connector) {
        this.previous = owner;
        this.hasList = hasList;
        this.hasNext = hasNext;
        this.connector = connector;
        this.first = true;
        moveCursor();
    }

    private void moveCursor() {
        this.cursor = connector.find(previous, first ? hasList : hasNext, null);
    }

    boolean hasNext() {
        return !cursor.isEmpty();
    }

    Axiom<NamedResource> next() {
        verifySuccessorCount();
        final NamedResource subject = NamedResource.create(previous.getURI());
        final Assertion assertion = Assertion
                .createObjectPropertyAssertion(URI.create(first ? hasList.getURI() : hasNext.getURI()), false);
        final Resource item = cursor.iterator().next().getObject().asResource();
        this.first = false;
        this.previous = item;
        final NamedResource value = NamedResource.create(item.getURI());
        moveCursor();
        return new AxiomImpl<>(subject, assertion, new Value<>(value));
    }

    private void verifySuccessorCount() {
        if (!hasNext()) {
            throw new NoSuchElementException("No more elements available.");
        }
        if (cursor.size() > 1) {
            throw new IntegrityConstraintViolatedException(
                    "Encountered multiple successors of list node " + previous.getURI());
        }
    }
}
