package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.*;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.createStatement;

class SimpleListIterator extends AbstractListIterator {

    SimpleListIterator(SimpleListDescriptor descriptor, StorageConnector connector) {
        super(descriptor, connector);
    }

    Axiom<NamedResource> nextAxiom() {
        final NamedResource subject = NamedResource.create(currentNode.getURI());
        final Assertion assertion = Assertion
                .createObjectPropertyAssertion(URI.create(first() ? hasList.getURI() : hasNext.getURI()), false);
        final NamedResource value = nextValue();
        return new AxiomImpl<>(subject, assertion, new Value<>(value));
    }

    NamedResource nextValue() {
        resolveNextListNode();
        return NamedResource.create(currentNode.getURI());
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
        toAdd.add(createStatement(previousNode, index == 0 ? hasList : hasNext, replacement));
        if (hasNext()) {
            verifySuccessorCount();
            final RDFNode nextNode = cursor.iterator().next().getObject();
            remove(currentNode, hasNext, nextNode);
            if (!nextNode.equals(replacement)) {
                final Statement linkToNext = createStatement(replacement, hasNext, nextNode);
                toAdd.add(linkToNext);
                this.cursor = Collections.singleton(linkToNext);
            } else {
                moveCursor(replacement);
            }
        }
        this.currentNode = replacement;
        if (context != null) {
            connector.add(toAdd, context);
        } else {
            connector.add(toAdd);
        }
    }
}
