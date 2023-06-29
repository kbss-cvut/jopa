/**
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.util.Procedure;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

class JenaProperties implements Properties {

    private final JenaAdapter adapter;

    private final Procedure beforeCallback;
    private final Procedure afterChangeCallback;

    JenaProperties(JenaAdapter adapter, Procedure beforeCallback, Procedure afterChangeCallback) {
        this.adapter = adapter;
        this.beforeCallback = beforeCallback;
        this.afterChangeCallback = afterChangeCallback;
    }

    @Override
    public Collection<Axiom<?>> getProperties(NamedResource individual, URI context, boolean includeInferred)
            throws JenaDriverException {
        Objects.requireNonNull(individual);
        beforeCallback.execute();
        return adapter.propertiesHandler().getProperties(individual, context, includeInferred);
    }

    @Override
    public void addProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws JenaDriverException {
        Objects.requireNonNull(individual);
        Objects.requireNonNull(properties);
        beforeCallback.execute();
        adapter.propertiesHandler().addProperties(individual, context, properties);
        afterChangeCallback.execute();
    }

    @Override
    public void removeProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws JenaDriverException {
        Objects.requireNonNull(individual);
        Objects.requireNonNull(properties);
        beforeCallback.execute();
        adapter.propertiesHandler().removeProperties(individual, context, properties);
        afterChangeCallback.execute();
    }
}
