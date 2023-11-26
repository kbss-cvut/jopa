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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.Lists;
import cz.cvut.kbss.ontodriver.descriptor.*;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;

import java.util.List;
import java.util.Objects;

import static cz.cvut.kbss.ontodriver.util.ErrorUtils.getNPXMessageSupplier;

class Rdf4jLists implements Lists {

    private final Rdf4jAdapter adapter;

    private final Procedure beforeCallback;
    private final Procedure afterChangeCallback;

    Rdf4jLists(Rdf4jAdapter adapter, Procedure beforeCallback, Procedure afterChangeCallback) {
        this.adapter = adapter;
        this.beforeCallback = beforeCallback;
        this.afterChangeCallback = afterChangeCallback;
    }

    @Override
    public List<Axiom<NamedResource>> loadSimpleList(SimpleListDescriptor descriptor)
            throws OntoDriverException {
        verifyArgs(descriptor);
        return adapter.getSimpleListHandler().loadList(descriptor);
    }

    private void verifyArgs(ListDescriptor descriptor) throws Rdf4jDriverException {
        beforeCallback.execute();
        Objects.requireNonNull(descriptor, getNPXMessageSupplier("descriptor"));
    }

    @Override
    public void persistSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException {
        verifyArgs(descriptor);
        adapter.getSimpleListHandler().persistList(descriptor);
        afterChangeCallback.execute();
    }

    @Override
    public void updateSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException {
        verifyArgs(descriptor);
        adapter.getSimpleListHandler().updateList(descriptor);
        afterChangeCallback.execute();
    }

    @Override
    public List<Axiom<NamedResource>> loadReferencedList(ReferencedListDescriptor descriptor)
            throws OntoDriverException {
        verifyArgs(descriptor);
        return adapter.getReferencedListHandler().loadList(descriptor);
    }

    @Override
    public <T> void persistReferencedList(ReferencedListValueDescriptor<T> descriptor)
            throws OntoDriverException {
        verifyArgs(descriptor);
        adapter.getReferencedListHandler().persistList(descriptor);
        afterChangeCallback.execute();
    }

    @Override
    public void updateReferencedList(ReferencedListValueDescriptor descriptor)
            throws OntoDriverException {
        verifyArgs(descriptor);
        adapter.getReferencedListHandler().updateList(descriptor);
        afterChangeCallback.execute();
    }
}
