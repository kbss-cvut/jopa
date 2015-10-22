package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAddAxiom;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.AxiomImpl;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.util.ArrayList;
import java.util.List;

class SimpleListHandler extends ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> {

    SimpleListHandler(OwlapiAdapter adapter, OntologyStructures snapshot) {
        super(adapter, snapshot);
    }

    OwlapiListIterator iterator(SimpleListDescriptor descriptor) {
        final SimpleListIterator iterator;
        if (descriptor.getListProperty().isInferred() || descriptor.getNextNode().isInferred()) {
            iterator = new InferredSimpleListIterator(descriptor, snapshot, axiomAdapter);
        } else {
            iterator = new SimpleListIterator(descriptor, snapshot, axiomAdapter);
        }
        return iterator;
    }

    @Override
    public void persistList(SimpleListValueDescriptor descriptor) {
        if (descriptor.getValues().isEmpty()) {
            return;
        }
        owlapiAdapter.addTransactionalChanges(snapshot.getOntologyManager().applyChanges(createListAxioms(descriptor)));
    }

    private List<OWLOntologyChange> createListAxioms(SimpleListValueDescriptor descriptor) {
        final List<OWLOntologyChange> changes = new ArrayList<>(descriptor.getValues().size());
        NamedResource previous = descriptor.getListOwner();
        boolean first = true;
        for (NamedResource item : descriptor.getValues()) {
            final OWLAxiom axiom;
            if (first) {
                axiom = axiomAdapter.toOwlObjectPropertyAssertionAxiom(
                        new AxiomImpl<>(previous, descriptor.getListProperty(), new Value<>(item)));
                first = false;
            } else {
                axiom = axiomAdapter.toOwlObjectPropertyAssertionAxiom(
                        new AxiomImpl<>(previous, descriptor.getNextNode(), new Value<>(item)));
            }
            previous = item;
            changes.add(new MutableAddAxiom(snapshot.getOntology(), axiom));
        }
        return changes;
    }
}
