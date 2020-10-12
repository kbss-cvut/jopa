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
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.util.Procedure;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Collection;
import java.util.Objects;
import java.util.Set;

public class JenaTypes implements Types {

    private final Procedure beforeCallback;
    private final Procedure afterCallback;

    private final JenaAdapter adapter;

    JenaTypes(JenaAdapter adapter, Procedure beforeCallback, Procedure afterCallback) {
        this.beforeCallback = beforeCallback;
        this.afterCallback = afterCallback;
        this.adapter = adapter;
    }

    @Override
    public Set<Axiom<URI>> getTypes(NamedResource individual, Collection<URI> contexts, boolean includeInferred)
            throws OntoDriverException {
        Objects.requireNonNull(individual);
        beforeCallback.execute();
        return adapter.typesHandler().getTypes(individual, contexts, includeInferred);
    }

    @Override
    public void addTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException {
        Objects.requireNonNull(individual, "individual");
        Objects.requireNonNull(types, "types");
        beforeCallback.execute();
        adapter.typesHandler().addTypes(individual, context, types);
        afterCallback.execute();
    }

    @Override
    public void removeTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException {
        Objects.requireNonNull(individual, "individual");
        Objects.requireNonNull(types, "types");
        beforeCallback.execute();
        adapter.typesHandler().removeTypes(individual, context, types);
        afterCallback.execute();
    }
}
