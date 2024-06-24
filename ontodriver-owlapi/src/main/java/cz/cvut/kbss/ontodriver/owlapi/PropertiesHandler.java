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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

class PropertiesHandler {

    private final OwlapiAdapter adapter;
    private final OntologySnapshot snapshot;

    PropertiesHandler(OwlapiAdapter adapter, OntologySnapshot snapshot) {
        this.adapter = adapter;
        this.snapshot = snapshot;
    }

    public Collection<Axiom<?>> getProperties(NamedResource subject, boolean includeInferred) {
        if (includeInferred) {
            return getPropertiesIncludingInferred(subject);
        } else {
            return getExplicitProperties(subject);
        }
    }

    private Collection<Axiom<?>> getPropertiesIncludingInferred(NamedResource subject) {
        return new InferredAxiomLoader(adapter, snapshot).loadPropertyAxioms(subject);
    }

    private Collection<Axiom<?>> getExplicitProperties(NamedResource subject) {
        return new ExplicitAxiomLoader(adapter, snapshot).loadPropertyAxioms(subject);
    }

    public void addProperties(NamedResource subject, Map<Assertion, Set<Value<?>>> properties) {
        new AxiomSaver(adapter, snapshot).persistAxioms(subject, properties);
    }

    public void removeProperties(NamedResource subject, Map<Assertion, Set<Value<?>>> properties) {
        new EpistemicAxiomRemover(adapter, snapshot).removeAxioms(subject, properties);
    }
}
