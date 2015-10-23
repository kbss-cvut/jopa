package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAddAxiom;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.AxiomImpl;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

class SimpleListHandler extends ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> {

    private final OWLOntologyManager ontologyManager;
    private final OWLOntology ontology;

    SimpleListHandler(OwlapiAdapter adapter, OntologyStructures snapshot) {
        super(adapter, snapshot);
        this.ontology = snapshot.getOntology();
        this.ontologyManager = snapshot.getOntologyManager();
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
        owlapiAdapter.addTransactionalChanges(ontologyManager.applyChanges(createListAxioms(descriptor)));
    }

    private List<OWLOntologyChange> createListAxioms(SimpleListValueDescriptor descriptor) {
        final List<OWLOntologyChange> changes = new ArrayList<>(descriptor.getValues().size());
        NamedResource previous = descriptor.getListOwner();
        boolean first = true;
        for (NamedResource item : descriptor.getValues()) {
            final OWLAxiom axiom;
            if (first) {
                axiom = appendNode(previous, descriptor.getListProperty(), item);
                first = false;
            } else {
                axiom = appendNode(previous, descriptor.getNextNode(), item);
            }
            previous = item;
            changes.add(new MutableAddAxiom(ontology, axiom));
        }
        return changes;
    }

    private OWLAxiom appendNode(NamedResource current, Assertion property, NamedResource next) {
        return axiomAdapter.toOwlObjectPropertyAssertionAxiom(new AxiomImpl<>(current, property, new Value<>(next)));
    }

    @Override
    public void updateList(SimpleListValueDescriptor descriptor) {
        if (descriptor.getValues().isEmpty()) {
            clearList(descriptor);
        } else if (isOrigEmpty(descriptor)) {
            persistList(descriptor);
        } else {
            mergeLists(descriptor);
        }
    }

    private void clearList(SimpleListDescriptor descriptor) {
        final OwlapiListIterator it = iterator(descriptor);
        final List<OWLOntologyChange> changes = new ArrayList<>();
        while (it.hasNext()) {
            it.next();
            changes.addAll(it.removeWithoutReconnect());
        }
        owlapiAdapter.addTransactionalChanges(ontologyManager.applyChanges(changes));
    }

    private boolean isOrigEmpty(SimpleListDescriptor descriptor) {
        final OwlapiListIterator it = iterator(descriptor);
        return !it.hasNext();
    }

    private void mergeLists(SimpleListValueDescriptor descriptor) {
        final OwlapiListIterator it = iterator(descriptor);
        final List<NamedResource> values = descriptor.getValues();
        final List<OWLOntologyChange> changes = new ArrayList<>(values.size());
        int i = 0;
        NamedResource lastNode = null;
        while (it.hasNext() && i < values.size()) {
            final NamedResource newValue = values.get(i);
            final NamedResource currentValue = it.nextValue();
            if (!newValue.equals(currentValue)) {
                changes.addAll(ontologyManager.applyChanges(it.replaceNode(newValue)));
            }
            lastNode = newValue;
            i++;
        }
        owlapiAdapter.addTransactionalChanges(changes);
        changes.clear();
        assert lastNode != null;
        changes.addAll(addNewNodes(descriptor, i, lastNode));
        changes.addAll(removeObsoleteNodes(it));
        owlapiAdapter.addTransactionalChanges(ontologyManager.applyChanges(changes));
    }

    private List<OWLOntologyChange> addNewNodes(SimpleListValueDescriptor descriptor, int index,
                                                NamedResource lastNode) {
        if (index >= descriptor.getValues().size()) {
            return Collections.emptyList();
        }
        final List<OWLOntologyChange> changes = new ArrayList<>(descriptor.getValues().size() - index);
        for (; index < descriptor.getValues().size(); index++) {
            final NamedResource next = descriptor.getValues().get(index);
            changes.add(new MutableAddAxiom(ontology, appendNode(lastNode, descriptor.getNextNode(), next)));
            lastNode = next;
        }
        return changes;
    }

    private List<OWLOntologyChange> removeObsoleteNodes(OwlapiListIterator iterator) {
        if (!iterator.hasNext()) {
            return Collections.emptyList();
        }
        final List<OWLOntologyChange> changes = new ArrayList<>();
        while (iterator.hasNext()) {
            iterator.next();
            changes.addAll(iterator.removeWithoutReconnect());
        }
        return changes;
    }
}
