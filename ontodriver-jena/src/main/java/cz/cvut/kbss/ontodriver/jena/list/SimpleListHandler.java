package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;

import java.util.ArrayList;
import java.util.List;

class SimpleListHandler extends ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> {

    SimpleListHandler(StorageConnector connector) {
        super(connector);
    }

    @Override
    public List<Axiom<NamedResource>> loadList(SimpleListDescriptor descriptor) {
        final List<Axiom<NamedResource>> result = new ArrayList<>();
        final Resource owner = ResourceFactory.createResource(descriptor.getListOwner().getIdentifier().toString());
        final Property hasList = ResourceFactory
                .createProperty(descriptor.getListProperty().getIdentifier().toString());
        final Property hasNext = ResourceFactory.createProperty(descriptor.getNextNode().getIdentifier().toString());
        final String context = descriptor.getContext() != null ? descriptor.getContext().toString() : null;
        final SimpleListIterator it = new SimpleListIterator(owner, hasList, hasNext, context, connector);
        while (it.hasNext()) {
            result.add(it.next());
        }
        return result;
    }

    @Override
    void persistList(SimpleListValueDescriptor descriptor) {

    }

    @Override
    void updateList(SimpleListValueDescriptor descriptor) {

    }
}
