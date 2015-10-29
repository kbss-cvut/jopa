package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver_new.descriptors.*;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.util.ArrayList;
import java.util.List;

public abstract class ListHandler<D extends ListDescriptor, V extends ListValueDescriptor> {

    protected final OwlapiAdapter owlapiAdapter;
    protected final AxiomAdapter axiomAdapter;

    protected final OWLOntology ontology;

    protected final OntologySnapshot snapshot;

    protected ListHandler(OwlapiAdapter owlapiAdapter, OntologySnapshot snapshot) {
        this.owlapiAdapter = owlapiAdapter;
        this.axiomAdapter = new AxiomAdapter(snapshot.getDataFactory(), owlapiAdapter.getLanguage());
        this.snapshot = snapshot;
        this.ontology = snapshot.getOntology();
    }

    public List<Axiom<NamedResource>> loadList(D descriptor) {
        final List<Axiom<NamedResource>> list = new ArrayList<>();
        final OwlapiListIterator iterator = iterator(descriptor);
        while (iterator.hasNext()) {
            list.add(iterator.next());
        }
        return list;
    }

    public void persistList(V descriptor) {
        if (descriptor.getValues().isEmpty()) {
            return;
        }
        owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(createListAxioms(descriptor)));
    }

    abstract OwlapiListIterator iterator(ListDescriptor descriptor);

    abstract List<OWLOntologyChange> createListAxioms(V descriptor);

    public void updateList(V descriptor) {
        if (descriptor.getValues().isEmpty()) {
            removeObsoleteNodes(iterator(descriptor));
        } else if (isOrigEmpty(descriptor)) {
            persistList(descriptor);
        } else {
            mergeLists(descriptor);
        }
    }

    abstract boolean isOrigEmpty(V descriptor);

    private void mergeLists(V descriptor) {
        final OwlapiListIterator it = iterator(descriptor);
        final List<NamedResource> values = descriptor.getValues();
        final List<OWLOntologyChange> changes = new ArrayList<>(values.size());
        int i = 0;
        NamedResource lastNode = null;
        while (it.hasNext() && i < values.size()) {
            final NamedResource newValue = values.get(i);
            final NamedResource currentValue = it.nextValue();
            if (!newValue.equals(currentValue)) {
                changes.addAll(snapshot.applyChanges(it.replaceNode(newValue)));
            }
            lastNode = it.getCurrentNode();
            i++;
        }
        owlapiAdapter.addTransactionalChanges(changes);
        assert lastNode != null;
        removeObsoleteNodes(it);
        addNewNodes(descriptor, i, lastNode);
    }

    private void removeObsoleteNodes(OwlapiListIterator iterator) {
        if (!iterator.hasNext()) {
            return;
        }
        final List<OWLOntologyChange> changes = new ArrayList<>();
        while (iterator.hasNext()) {
            iterator.next();
            changes.addAll(iterator.removeWithoutReconnect());
        }
        owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(changes));
    }

    abstract void addNewNodes(V descriptor, int index, NamedResource lastNode);

    public static ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> getSimpleListHandler(
            OwlapiAdapter adapter, OntologySnapshot snapshot) {
        return new SimpleListHandler(adapter, snapshot);
    }

    public static ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> getReferencedListHandler(
            OwlapiAdapter adapter, OntologySnapshot snapshot) {
        return new ReferencedListHandler(adapter, snapshot);
    }
}
