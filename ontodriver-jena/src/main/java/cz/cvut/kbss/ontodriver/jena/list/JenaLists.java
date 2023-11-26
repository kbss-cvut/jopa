/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
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
    public <V> void persistReferencedList(ReferencedListValueDescriptor<V> descriptor) throws OntoDriverException {
        executeBeforeCallbackAndVerifyArgument(descriptor);
        adapter.referencedListHandler().persistList(descriptor);
        afterChangeCallback.execute();
    }

    @Override
    public <V> void updateReferencedList(ReferencedListValueDescriptor<V> descriptor) throws OntoDriverException {
        executeBeforeCallbackAndVerifyArgument(descriptor);
        adapter.referencedListHandler().updateList(descriptor);
        afterChangeCallback.execute();
    }
}
