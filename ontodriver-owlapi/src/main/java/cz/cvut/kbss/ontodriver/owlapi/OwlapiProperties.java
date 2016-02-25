/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import java.net.URI;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static cz.cvut.kbss.ontodriver.util.ErrorUtils.npxMessage;

public class OwlapiProperties implements Properties {

    private final OwlapiConnection connection;

    private final OwlapiAdapter adapter;

    public OwlapiProperties(OwlapiConnection connection, OwlapiAdapter adapter) {
        this.connection = connection;
        this.adapter = adapter;
    }

    @Override
    public Collection<Axiom<?>> getProperties(NamedResource individual, URI context, boolean includeInferred)
            throws OntoDriverException {
        Objects.requireNonNull(individual, npxMessage("individual"));
        connection.ensureOpen();
        return adapter.getPropertiesHandler().getProperties(individual, includeInferred);
    }

    @Override
    public void addProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws OntoDriverException {
        ensureValidity(individual, properties);
        if (!properties.isEmpty()) {
            adapter.getPropertiesHandler().addProperties(individual, properties);
        }
        connection.commitIfAuto();
    }

    private void ensureValidity(NamedResource individual, Map<Assertion, Set<Value<?>>> properties) {
        Objects.requireNonNull(individual, npxMessage("individual"));
        Objects.requireNonNull(properties, npxMessage("properties"));
        connection.ensureOpen();
    }

    @Override
    public void removeProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws OntoDriverException {
        ensureValidity(individual, properties);
        if (!properties.isEmpty()) {
            adapter.getPropertiesHandler().removeProperties(individual, properties);
        }
        connection.commitIfAuto();
    }
}
