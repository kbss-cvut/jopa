package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiConnection;
import cz.cvut.kbss.ontodriver.Lists;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.util.List;
import java.util.Objects;

/**
 * Public access point the simple and referenced list handling in the OWLAPI driver.
 */
public class OwlapiLists implements Lists {

    private final OwlapiConnection connection;
    private final OwlapiAdapter adapter;

    public OwlapiLists(OwlapiConnection connection, OwlapiAdapter adapter) {
        assert connection != null;
        assert adapter != null;
        this.connection = connection;
        this.adapter = adapter;
    }

    @Override
    public List<Axiom<NamedResource>> loadSimpleList(SimpleListDescriptor descriptor) throws OntoDriverException {
        ensureStateAndArgumentValid(descriptor);
        return adapter.getSimpleListHandler().loadList(descriptor);
    }

    private void ensureStateAndArgumentValid(Object argument) {
        connection.ensureOpen();
        Objects.requireNonNull(argument);
    }

    @Override
    public void persistSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException {
        ensureStateAndArgumentValid(descriptor);
        adapter.getSimpleListHandler().persistList(descriptor);
        connection.commitIfAuto();
    }

    @Override
    public void updateSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException {
        ensureStateAndArgumentValid(descriptor);
        adapter.getSimpleListHandler().updateList(descriptor);
        connection.commitIfAuto();
    }

    @Override
    public List<Axiom<NamedResource>> loadReferencedList(ReferencedListDescriptor descriptor)
            throws OntoDriverException {
        ensureStateAndArgumentValid(descriptor);
        return adapter.getReferencedListHandler().loadList(descriptor);
    }

    @Override
    public void persistReferencedList(ReferencedListValueDescriptor descriptor) throws OntoDriverException {
        ensureStateAndArgumentValid(descriptor);
        adapter.getReferencedListHandler().persistList(descriptor);
        connection.commitIfAuto();
    }

    @Override
    public void updateReferencedList(ReferencedListValueDescriptor descriptor) throws OntoDriverException {
        ensureStateAndArgumentValid(descriptor);
        adapter.getReferencedListHandler().updateList(descriptor);
        connection.commitIfAuto();
    }
}
