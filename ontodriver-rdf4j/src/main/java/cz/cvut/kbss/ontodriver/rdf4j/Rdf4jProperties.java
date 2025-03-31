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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.rdf4j.config.RuntimeConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.connector.RepoConnection;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;

import java.net.URI;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

public class Rdf4jProperties implements Properties {

    private final RepoConnection connector;
    private final RuntimeConfiguration config;

    private final Procedure beforeCallback;
    private final Procedure afterChangeCallback;

    public Rdf4jProperties(Rdf4jAdapter adapter, Procedure beforeCallback, Procedure afterChangeCallback) {
        this.connector = adapter.getConnector();
        this.config = adapter.getConfig();
        this.beforeCallback = beforeCallback;
        this.afterChangeCallback = afterChangeCallback;
    }

    @Override
    public Collection<Axiom<?>> getProperties(NamedResource individual, URI context, boolean includeInferred)
            throws Rdf4jDriverException {
        beforeCallback.execute();
        return new AxiomLoader(connector, config).loadAxioms(individual, includeInferred, context);
    }

    @Override
    public void addProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws OntoDriverException {
        beforeCallback.execute();
        new AxiomSaver(connector).persistAxioms(individual, properties, context);
        afterChangeCallback.execute();
    }

    @Override
    public void removeProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws OntoDriverException {
        new EpistemicAxiomRemover(connector, connector.getValueFactory()).remove(individual, properties, context);
        afterChangeCallback.execute();
    }
}
