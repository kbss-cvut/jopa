/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

import java.net.URI;
import java.util.Collection;
import java.util.Objects;
import java.util.Set;

import static cz.cvut.kbss.ontodriver.util.ErrorUtils.getNPXMessageSupplier;

public class SesameTypes implements Types {

    private final SesameAdapter adapter;

    private final Procedure beforeCallback;
    private final Procedure afterChangeCallback;

    public SesameTypes(SesameAdapter adapter, Procedure beforeCallback, Procedure afterChangeCallback) {
        this.adapter = adapter;
        this.beforeCallback = beforeCallback;
        this.afterChangeCallback = afterChangeCallback;
    }

    @Override
    public Set<Axiom<URI>> getTypes(NamedResource individual, Collection<URI> contexts, boolean includeInferred)
            throws OntoDriverException {
        Objects.requireNonNull(individual, getNPXMessageSupplier("individual"));
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

    private void verifyValidity(NamedResource individual, Set<URI> types) throws SesameDriverException {
        Objects.requireNonNull(individual, getNPXMessageSupplier("individual"));
        Objects.requireNonNull(types, getNPXMessageSupplier("types"));
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
