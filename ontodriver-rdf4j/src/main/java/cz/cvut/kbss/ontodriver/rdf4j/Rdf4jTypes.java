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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;

import java.net.URI;
import java.util.Collection;
import java.util.Objects;
import java.util.Set;

public class Rdf4jTypes implements Types {

    private final Rdf4jAdapter adapter;

    private final Procedure beforeCallback;
    private final Procedure afterChangeCallback;

    public Rdf4jTypes(Rdf4jAdapter adapter, Procedure beforeCallback, Procedure afterChangeCallback) {
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
        verifyValidity(individual, types);
        if (!types.isEmpty()) {
            adapter.getTypesHandler().addTypes(individual, context, types);
        }
        afterChangeCallback.execute();
    }

    private void verifyValidity(NamedResource individual, Set<URI> types) throws Rdf4jDriverException {
        Objects.requireNonNull(individual);
        Objects.requireNonNull(types);
        beforeCallback.execute();
    }

    @Override
    public void removeTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException {
        verifyValidity(individual, types);
        if (!types.isEmpty()) {
            adapter.getTypesHandler().removeTypes(individual, context, types);
        }
        afterChangeCallback.execute();
    }
}
