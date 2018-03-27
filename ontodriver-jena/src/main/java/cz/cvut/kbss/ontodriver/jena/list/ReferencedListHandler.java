package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.util.List;

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

    }

    @Override
    void updateList(ReferencedListValueDescriptor descriptor) {

    }
}
