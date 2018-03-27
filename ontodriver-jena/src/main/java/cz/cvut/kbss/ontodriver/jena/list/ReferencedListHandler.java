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
        final Property hasNext = createProperty(descriptor.getNextNode().getIdentifier().toString());
        final Property hasContent = createProperty(descriptor.getNodeContent().getIdentifier().toString());
        final String context = descriptor.getContext() != null ? descriptor.getContext().toString() : null;
        Resource previous = createResource(descriptor.getListOwner().getIdentifier().toString());
        final List<Statement> toAdd = new ArrayList<>(values.size() * 2);
        for (int i = 0; i < values.size(); i++) {
            final Resource node = generateNewListNode(descriptor.getListProperty().getIdentifier(), i, context);
            if (i == 0) {
                toAdd.add(createStatement(previous,
                        createProperty(descriptor.getListProperty().getIdentifier().toString()), node));
            } else {
                toAdd.add(createStatement(previous, hasNext, node));
            }
            toAdd.add(createStatement(node, hasContent, createResource(values.get(i).getIdentifier().toString())));
            previous = node;
        }
        if (context != null) {
            connector.add(toAdd, context);
        } else {
            connector.add(toAdd);
        }
    }

    private Resource generateNewListNode(URI property, int index, String context) {
        Resource node;
        Collection<Statement> statements;
        do {
            node = createResource(property.toString() + "-SEQ_" + index++);
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
        final AbstractListIterator it = new ReferencedListIterator(descriptor, connector);
        int i = 0;
        while (it. hasNext() && i < descriptor.getValues().size()) {
            final NamedResource value = it.nextValue();
            i++;
        }
        if (i < descriptor.getValues().size()) {

        }
    }
}
