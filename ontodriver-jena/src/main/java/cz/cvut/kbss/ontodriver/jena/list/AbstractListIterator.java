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

    final Property hasList;
    final Property hasNext;

    final String context;

    int index;
    boolean removed = false;

    Resource previousNode;
    Resource currentNode;
    Collection<Statement> cursor;

    AbstractListIterator(ListDescriptor descriptor, StorageConnector connector) {
        this.hasList = createProperty(descriptor.getListProperty().getIdentifier().toString());
        this.hasNext = createProperty(descriptor.getNextNode().getIdentifier().toString());
        this.context = descriptor.getContext() != null ? descriptor.getContext().toString() : null;
        this.connector = connector;
        this.index = -1;
        this.currentNode = createResource(descriptor.getListOwner().getIdentifier().toString());
        moveCursor(currentNode);
    }

    void moveCursor(Resource from) {
        if (context != null) {
            this.cursor = connector.find(from, first() ? hasList : hasNext, null, context);
        } else {
            this.cursor = connector.find(from, first() ? hasList : hasNext, null);
        }
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

    abstract Axiom<NamedResource> nextAxiom();

    abstract NamedResource nextValue();

    abstract void replace(Resource replacement);
}
