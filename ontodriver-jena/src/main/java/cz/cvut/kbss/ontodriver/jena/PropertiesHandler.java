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
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

class PropertiesHandler implements Properties {

    private final StorageConnector connector;

    PropertiesHandler(StorageConnector connector) {
        this.connector = connector;
    }

    @Override
    public Collection<Axiom<?>> getProperties(NamedResource individual, URI context, boolean includeInferred) {
        return new ExplicitAxiomLoader(connector).find(individual, context);
    }

    @Override
    public void addProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties) {
        new AxiomSaver(connector).saveAxioms(individual, properties, context);
    }

    @Override
    public void removeProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties) {
        new EpistemicAxiomRemover(connector).remove(individual, properties, context);
    }
}
