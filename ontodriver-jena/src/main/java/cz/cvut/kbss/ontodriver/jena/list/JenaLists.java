package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.Lists;
import cz.cvut.kbss.ontodriver.descriptor.*;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.JenaAdapter;
import cz.cvut.kbss.ontodriver.jena.util.Procedure;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.util.List;
import java.util.Objects;

public class JenaLists implements Lists {

    private final JenaAdapter adapter;

    private final Procedure beforeCallback;
    private final Procedure afterChangeCallback;

    public JenaLists(JenaAdapter adapter, Procedure beforeCallback, Procedure afterChangeCallback) {
        this.adapter = adapter;
        this.beforeCallback = beforeCallback;
        this.afterChangeCallback = afterChangeCallback;
    }

    @Override
    public List<Axiom<NamedResource>> loadSimpleList(SimpleListDescriptor descriptor) throws OntoDriverException {
        executeBeforeCallbackAndVerifyArgument(descriptor);
        Objects.requireNonNull(descriptor);
        return adapter.simpleListHandler().loadList(descriptor);
    }

    private void executeBeforeCallbackAndVerifyArgument(ListDescriptor descriptor) throws OntoDriverException {
        beforeCallback.execute();
        Objects.requireNonNull(descriptor);
    }

    @Override
    public void persistSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException {
        executeBeforeCallbackAndVerifyArgument(descriptor);
        adapter.simpleListHandler().persistList(descriptor);
        afterChangeCallback.execute();
    }

    @Override
    public void updateSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException {
        executeBeforeCallbackAndVerifyArgument(descriptor);
        adapter.simpleListHandler().updateList(descriptor);
        afterChangeCallback.execute();
    }

    @Override
    public List<Axiom<NamedResource>> loadReferencedList(ReferencedListDescriptor descriptor)
            throws OntoDriverException {
        executeBeforeCallbackAndVerifyArgument(descriptor);
        return adapter.referencedListHandler().loadList(descriptor);
    }

    @Override
    public void persistReferencedList(ReferencedListValueDescriptor descriptor) throws OntoDriverException {
        executeBeforeCallbackAndVerifyArgument(descriptor);
        adapter.referencedListHandler().persistList(descriptor);
        afterChangeCallback.execute();
    }

    @Override
    public void updateReferencedList(ReferencedListValueDescriptor descriptor) throws OntoDriverException {
        executeBeforeCallbackAndVerifyArgument(descriptor);
        adapter.referencedListHandler().updateList(descriptor);
        afterChangeCallback.execute();
    }
}
