package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver_new.descriptors.*;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.util.ArrayList;
import java.util.List;

public abstract class ListHandler<D extends ListDescriptor, V extends ListValueDescriptor> {

    protected final OwlapiAdapter owlapiAdapter;
    protected final AxiomAdapter axiomAdapter;
    protected final OntologyStructures snapshot;

    protected ListHandler(OwlapiAdapter owlapiAdapter, OntologyStructures snapshot) {
        this.owlapiAdapter = owlapiAdapter;
        this.axiomAdapter = new AxiomAdapter(snapshot.getDataFactory(), owlapiAdapter.getLanguage());
        this.snapshot = snapshot;
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
        owlapiAdapter.addTransactionalChanges(snapshot.getOntologyManager().applyChanges(createListAxioms(descriptor)));
    }

    abstract OwlapiListIterator iterator(D descriptor);

    abstract List<OWLOntologyChange> createListAxioms(V descriptor);

    public abstract void updateList(V descriptor);

    public static ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> getSimpleListHandler(
            OwlapiAdapter adapter, OntologyStructures snapshot) {
        return new SimpleListHandler(adapter, snapshot);
    }

    public static ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> getReferencedListHandler(
            OwlapiAdapter adapter, OntologyStructures snapshot) {
        return new ReferencedListHandler(adapter, snapshot);
    }
}
