package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Resource;

import java.util.ArrayList;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;

public abstract class ListHandler<D extends ListDescriptor, V extends ListValueDescriptor> {

    final StorageConnector connector;

    ListHandler(StorageConnector connector) {
        this.connector = connector;
    }

    List<Axiom<NamedResource>> loadList(D descriptor) {
        final List<Axiom<NamedResource>> result = new ArrayList<>();
        final AbstractListIterator it = iterator(descriptor);
        while (it.hasNext()) {
            result.add(it.nextAxiom());
        }
        return result;
    }

    abstract AbstractListIterator iterator(D descriptor);

    abstract AbstractListIterator iterator(V descriptor);

    void persistList(V descriptor) {
        final List<NamedResource> values = descriptor.getValues();
        if (values.isEmpty()) {
            return;
        }
        Resource owner = createResource(descriptor.getListOwner().getIdentifier().toString());
        appendNewNodes(descriptor, 0, owner);
    }

    abstract void appendNewNodes(V descriptor, int index, Resource lastNode);

    void updateList(V descriptor) {
        final AbstractListIterator it = iterator(descriptor);
        int i = 0;
        while (it.hasNext() && i < descriptor.getValues().size()) {
            final NamedResource update = descriptor.getValues().get(i);
            final NamedResource existing = it.nextValue();
            if (!existing.equals(update)) {
                it.replace(createResource(update.getIdentifier().toString()));
            }
            i++;
        }
        removeObsoleteNodes(it);
        if (i < descriptor.getValues().size()) {
            appendNewNodes(descriptor, i, it.getCurrentNode());
        }
    }

    private static void removeObsoleteNodes(AbstractListIterator it) {
        while (it.hasNext()) {
            it.nextValue();
            it.removeWithoutReconnect();
        }
    }

    public static SimpleListHandler simpleListHandler(StorageConnector connector) {
        return new SimpleListHandler(connector);
    }

    public static ReferencedListHandler referencedListHandler(StorageConnector connector) {
        return new ReferencedListHandler(connector);
    }
}
