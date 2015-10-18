package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver_new.descriptors.ListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

import java.util.List;

public abstract class ListHandler<D extends ListDescriptor, V extends ListValueDescriptor> {

    protected final OwlapiAdapter owlapiAdapter;

    protected ListHandler(OwlapiAdapter owlapiAdapter) {
        this.owlapiAdapter = owlapiAdapter;
    }

    public abstract List<Axiom<NamedResource>> loadList(D descriptor);

    public abstract void persistList(V descriptor);

    public static ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> getSimpleListHandler(
            OntologyStructures snapshot, OwlapiAdapter adapter) {
        return new SimpleListHandler(snapshot, adapter);
    }
}
