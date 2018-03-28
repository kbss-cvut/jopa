package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.*;

class ReferencedListHandler extends ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> {

    ReferencedListHandler(StorageConnector connector) {
        super(connector);
    }

    @Override
    AbstractListIterator iterator(ReferencedListDescriptor descriptor) {
        return new ReferencedListIterator(descriptor, connector);
    }

    @Override
    void persistList(ReferencedListValueDescriptor descriptor) {
        final List<NamedResource> values = descriptor.getValues();
        if (values.isEmpty()) {
            return;
        }
        Resource owner = createResource(descriptor.getListOwner().getIdentifier().toString());
        appendNewNodes(descriptor, 0, owner);
    }

    private void appendNewNodes(ReferencedListValueDescriptor descriptor, int i, Resource lastNode) {
        assert lastNode != null;
        final List<Statement> toAdd = new ArrayList<>((descriptor.getValues().size() - i) * 2);
        final Property hasList = createProperty(descriptor.getListProperty().getIdentifier().toString());
        final Property hasNext = createProperty(descriptor.getNextNode().getIdentifier().toString());
        final Property hasContent = createProperty(descriptor.getNodeContent().getIdentifier().toString());
        final String context = descriptor.getContext() != null ? descriptor.getContext().toString() : null;
        for (; i < descriptor.getValues().size(); i++) {
            lastNode =
                    appendNode(lastNode, descriptor.getValues().get(i), i == 0 ? hasList : hasNext, hasContent, context,
                            toAdd);
        }
        if (context != null) {
            connector.add(toAdd, context);
        } else {
            connector.add(toAdd);
        }
    }

    private Resource appendNode(Resource previousNode, NamedResource value, Property link, Property hasContent,
                                String context, List<Statement> statements) {
        final Resource node = generateNewListNode(value.getIdentifier(), context);
        statements.add(createStatement(previousNode, link, node));
        statements.add(createStatement(node, hasContent, createResource(value.getIdentifier().toString())));
        return node;
    }

    private Resource generateNewListNode(URI baseUri, String context) {
        Resource node;
        int index = 0;
        Collection<Statement> statements;
        do {
            node = createResource(baseUri.toString() + "-SEQ_" + index++);
            if (context != null) {
                statements = connector.find(node, null, null, context);
            } else {
                statements = connector.find(node, null, null);
            }
        } while (!statements.isEmpty());
        return node;
    }

    @Override
    void updateList(ReferencedListValueDescriptor descriptor) {
        final ReferencedListIterator it = new ReferencedListIterator(descriptor, connector);
        int i = 0;
        Resource lastNode = createResource(descriptor.getListOwner().getIdentifier().toString());
        while (it.hasNext() && i < descriptor.getValues().size()) {
            final NamedResource value = it.nextValue();
            if (!value.equals(descriptor.getValues().get(i))) {
                it.replace(createResource(descriptor.getValues().get(i).getIdentifier().toString()));
            }
            lastNode = it.getCurrentNode();
            i++;
        }
        removeObsoleteNodes(it);
        if (i < descriptor.getValues().size()) {
            appendNewNodes(descriptor, i, lastNode);
        }
    }

    private void removeObsoleteNodes(ReferencedListIterator it) {
        while (it.hasNext()) {
            it.nextValue();
            it.removeWithoutReconnect();
        }
    }
}
