/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.util.Procedure;

import java.net.URI;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static cz.cvut.kbss.ontodriver.util.ErrorUtils.getNPXMessageSupplier;

public class OwlapiProperties implements Properties {

    private final OwlapiAdapter adapter;

    private final Procedure beforeCallback;
    private final Procedure afterChangeCallback;

    public OwlapiProperties(OwlapiAdapter adapter, Procedure beforeCallback, Procedure afterChangeCallback) {
        this.afterChangeCallback = afterChangeCallback;
        this.beforeCallback = beforeCallback;
        this.adapter = adapter;
    }

    @Override
    public Collection<Axiom<?>> getProperties(NamedResource individual, URI context, boolean includeInferred)
            throws OntoDriverException {
        Objects.requireNonNull(individual, getNPXMessageSupplier("individual"));
        beforeCallback.execute();
        return adapter.getPropertiesHandler().getProperties(individual, includeInferred);
    }

    @Override
    public void addProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws OntoDriverException {
        ensureValidity(individual, properties);
        if (!properties.isEmpty()) {
            adapter.getPropertiesHandler().addProperties(individual, properties);
        }
        afterChangeCallback.execute();
    }

    private void ensureValidity(NamedResource individual, Map<Assertion, Set<Value<?>>> properties)
            throws OwlapiDriverException {
        Objects.requireNonNull(individual, getNPXMessageSupplier("individual"));
        Objects.requireNonNull(properties, getNPXMessageSupplier("properties"));
        beforeCallback.execute();
    }

    @Override
    public void removeProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws OntoDriverException {
        ensureValidity(individual, properties);
        if (!properties.isEmpty()) {
            adapter.getPropertiesHandler().removeProperties(individual, properties);
        }
        afterChangeCallback.execute();
    }
}
