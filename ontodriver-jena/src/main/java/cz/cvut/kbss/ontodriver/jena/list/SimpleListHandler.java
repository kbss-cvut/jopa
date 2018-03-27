package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;

class SimpleListHandler extends ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> {

    SimpleListHandler(StorageConnector connector) {
        super(connector);
    }

    @Override
    SimpleListIterator iterator(SimpleListDescriptor descriptor) {
        return new SimpleListIterator(descriptor, connector);
    }

    @Override
    void persistList(SimpleListValueDescriptor descriptor) {
        if (descriptor.getValues().isEmpty()) {
            return;
        }
        insert(axiomsToStatements(descriptor), descriptor.getContext());
    }

    private List<Statement> axiomsToStatements(SimpleListValueDescriptor descriptor) {
        final List<Statement> statements = new ArrayList<>(descriptor.getValues().size());
        final Resource owner = createResource(descriptor.getListOwner().getIdentifier().toString());
        final Property hasList = ResourceFactory
                .createProperty(descriptor.getListProperty().getIdentifier().toString());
        final Property hasNext = ResourceFactory.createProperty(descriptor.getNextNode().getIdentifier().toString());
        Resource previous = owner;
        boolean first = true;
        for (NamedResource value : descriptor.getValues()) {
            final Resource node = createResource(value.getIdentifier().toString());
            if (first) {
                statements.add(ResourceFactory.createStatement(previous, hasList, node));
                first = false;
            } else {
                statements.add(ResourceFactory.createStatement(previous, hasNext, node));
            }
            previous = node;
        }
        return statements;
    }

    private void insert(List<Statement> statements, URI context) {
        if (context != null) {
            connector.add(statements, context.toString());
        } else {
            connector.add(statements);
        }
    }

    @Override
    void updateList(SimpleListValueDescriptor descriptor) {
        final SimpleListIterator it = iterator(descriptor);
        if (!it.hasNext() && !descriptor.getValues().isEmpty()) {
            persistList(descriptor);
        } else {
            mergeLists(descriptor, it);
        }
    }

    private void mergeLists(SimpleListValueDescriptor descriptor, SimpleListIterator it) {
        int i = 0;
        NamedResource current = null;
        while (it.hasNext() && i < descriptor.getValues().size()) {
            current = it.nextValue();
            final NamedResource update = descriptor.getValues().get(i);
            if (!current.equals(update)) {
                it.replace(createResource(update.getIdentifier().toString()));
                current = update;
            }
            i++;
        }
        removeObsoleteNodes(it);
        if (descriptor.getValues().size() > i) {
            assert current != null;
            persistAdditionalNodes(current, descriptor, i);
        }
    }

    private void removeObsoleteNodes(SimpleListIterator it) {
        while (it.hasNext()) {
            it.nextValue();
            it.removeWithoutReconnect();
        }
    }

    private void persistAdditionalNodes(NamedResource lastExisting, SimpleListValueDescriptor descriptor, int index) {
        final Property hasNext = ResourceFactory.createProperty(descriptor.getNextNode().getIdentifier().toString());
        Resource previous = createResource(lastExisting.getIdentifier().toString());
        final List<NamedResource> values = descriptor.getValues();
        final List<Statement> toAdd = new ArrayList<>(values.size() - index);
        for (int i = index; i < values.size(); i++) {
            final Resource current = createResource(values.get(i).getIdentifier().toString());
            if (current.equals(previous)) {
                continue;
            }
            toAdd.add(ResourceFactory.createStatement(previous, hasNext, current));
            previous = current;
        }
        insert(toAdd, descriptor.getContext());
    }
}
