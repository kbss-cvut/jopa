/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.Lists;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.util.Procedure;

import java.util.List;
import java.util.Objects;

/**
 * Public access point the simple and referenced list handling in the OWLAPI driver.
 */
public class OwlapiLists implements Lists {

    private final OwlapiAdapter adapter;

    private final Procedure beforeCallback;
    private final Procedure afterChangeCallback;

    public OwlapiLists(OwlapiAdapter adapter, Procedure beforeCallback, Procedure afterChangeCallback) {
        this.adapter = adapter;
        this.beforeCallback = beforeCallback;
        this.afterChangeCallback = afterChangeCallback;
    }

    @Override
    public List<Axiom<NamedResource>> loadSimpleList(SimpleListDescriptor descriptor) throws OntoDriverException {
        ensureStateAndArgumentValid(descriptor);
        return adapter.getSimpleListHandler().loadList(descriptor);
    }

    private void ensureStateAndArgumentValid(Object argument) throws OwlapiDriverException {
        beforeCallback.execute();
        Objects.requireNonNull(argument);
    }

    @Override
    public void persistSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException {
        ensureStateAndArgumentValid(descriptor);
        adapter.getSimpleListHandler().persistList(descriptor);
        afterChangeCallback.execute();
    }

    @Override
    public void updateSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException {
        ensureStateAndArgumentValid(descriptor);
        adapter.getSimpleListHandler().updateList(descriptor);
        afterChangeCallback.execute();
    }

    @Override
    public List<Axiom<?>> loadReferencedList(ReferencedListDescriptor descriptor)
            throws OntoDriverException {
        ensureStateAndArgumentValid(descriptor);
        return adapter.getReferencedListHandler().loadList(descriptor);
    }

    @Override
    public <V> void persistReferencedList(ReferencedListValueDescriptor<V> descriptor) throws OntoDriverException {
        ensureStateAndArgumentValid(descriptor);
        adapter.getReferencedListHandler().persistList(descriptor);
        afterChangeCallback.execute();
    }

    @Override
    public <V> void updateReferencedList(ReferencedListValueDescriptor<V> descriptor) throws OntoDriverException {
        ensureStateAndArgumentValid(descriptor);
        adapter.getReferencedListHandler().updateList(descriptor);
        afterChangeCallback.execute();
    }
}
