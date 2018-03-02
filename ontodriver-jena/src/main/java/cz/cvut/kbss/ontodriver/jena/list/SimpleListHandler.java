package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;

import java.util.ArrayList;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;

class SimpleListHandler extends ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> {

    SimpleListHandler(StorageConnector connector) {
        super(connector);
    }

    @Override
    public List<Axiom<NamedResource>> loadList(SimpleListDescriptor descriptor) {
        final List<Axiom<NamedResource>> result = new ArrayList<>();
        final Resource owner = createResource(descriptor.getListOwner().getIdentifier().toString());
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
        if (descriptor.getValues().isEmpty()) {
            return;
        }
        if (descriptor.getContext() != null) {
            connector.add(axiomsToStatements(descriptor), descriptor.getContext().toString());
        } else {
            connector.add(axiomsToStatements(descriptor));
        }
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

    @Override
    void updateList(SimpleListValueDescriptor descriptor) {

    }
}
