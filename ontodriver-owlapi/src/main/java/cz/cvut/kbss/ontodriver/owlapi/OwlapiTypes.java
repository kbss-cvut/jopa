/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.util.Procedure;

import java.net.URI;
import java.util.Collection;
import java.util.Objects;
import java.util.Set;

public class OwlapiTypes implements Types {

    private final OwlapiAdapter adapter;

    private final Procedure beforeCallback;
    private final Procedure afterChangeCallback;

    public OwlapiTypes(OwlapiAdapter adapter, Procedure beforeCallback, Procedure afterChangeCallback) {
        this.adapter = adapter;
        this.beforeCallback = beforeCallback;
        this.afterChangeCallback = afterChangeCallback;
    }

    @Override
    public Set<Axiom<URI>> getTypes(NamedResource individual, Collection<URI> contexts, boolean includeInferred)
            throws OntoDriverException {
        Objects.requireNonNull(individual);
        beforeCallback.execute();
        return adapter.getTypesHandler().getTypes(individual, contexts, includeInferred);
    }

    @Override
    public void addTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException {
        ensureValidity(individual, types);
        if (!types.isEmpty()) {
            adapter.getTypesHandler().addTypes(individual, context, types);
        }
        afterChangeCallback.execute();
    }

    private void ensureValidity(NamedResource individual, Set<URI> types) throws OwlapiDriverException {
        Objects.requireNonNull(individual);
        Objects.requireNonNull(types);
        beforeCallback.execute();
    }

    @Override
    public void removeTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException {
        ensureValidity(individual, types);
        if (!types.isEmpty()) {
            adapter.getTypesHandler().removeTypes(individual, context, types);
        }
        afterChangeCallback.execute();
    }
}
