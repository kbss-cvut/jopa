package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.util.ArrayList;
import java.util.List;

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

    abstract void persistList(V descriptor);

    abstract void updateList(V descriptor);

    public static SimpleListHandler simpleListHandler(StorageConnector connector) {
        return new SimpleListHandler(connector);
    }

    public static ReferencedListHandler referencedListHandler(StorageConnector connector) {
        return new ReferencedListHandler(connector);
    }
}
