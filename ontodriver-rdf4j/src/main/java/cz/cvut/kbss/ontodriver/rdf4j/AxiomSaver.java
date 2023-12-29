/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
import cz.cvut.kbss.ontodriver.rdf4j.util.ValueConverter;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;

import java.net.URI;
import java.util.*;

class AxiomSaver {

    private final Connector connector;
    private final ValueConverter valueConverter;

    AxiomSaver(Connector connector) {
        this.connector = connector;
        this.valueConverter = new ValueConverter(connector.getValueFactory());
    }

    void persistAxioms(AxiomValueDescriptor axiomDescriptor) throws Rdf4jDriverException {
        final List<Statement> statements = new ArrayList<>();
        for (Assertion assertion : axiomDescriptor.getAssertions()) {
            statements.addAll(createRdf4jStatements(axiomDescriptor.getSubject(), assertion,
                                                    axiomDescriptor.getAssertionValues(assertion),
                                                    axiomDescriptor.getAssertionContext(assertion)));
        }
        if (!statements.isEmpty()) {
            connector.addStatements(statements);
        }
    }

    void persistAxioms(NamedResource subject, Map<Assertion, Set<Value<?>>> values, URI context)
            throws Rdf4jDriverException {
        final List<Statement> statements = new ArrayList<>();
        for (Map.Entry<Assertion, Set<Value<?>>> entry : values.entrySet()) {
            statements.addAll(createRdf4jStatements(subject, entry.getKey(), entry.getValue(), context));
        }
        if (!statements.isEmpty()) {
            connector.addStatements(statements);
        }
    }

    private Collection<? extends Statement> createRdf4jStatements(NamedResource subject,
                                                                  Assertion assertion,
                                                                  Collection<Value<?>> assertionValues,
                                                                  URI assertionContext) throws Rdf4jDriverException {
        final List<Statement> statements = new ArrayList<>(assertionValues.size());

        final Resource subjectUri = Rdf4jUtils.toRdf4jIri(subject.getIdentifier(), connector.getValueFactory());
        final IRI property = Rdf4jUtils.toRdf4jIri(assertion.getIdentifier(), connector.getValueFactory());
        final IRI context = assertionContext != null ? Rdf4jUtils.toRdf4jIri(assertionContext, connector.getValueFactory()) : null;
        for (Value<?> val : assertionValues) {
            if (val == Value.nullValue()) {
                continue;
            }
            org.eclipse.rdf4j.model.Value value = valueConverter.toRdf4jValue(assertion, val);
            statements.add(createStatement(subjectUri, property, value, context));
        }
        return statements;
    }

    private Statement createStatement(Resource subject, IRI property, org.eclipse.rdf4j.model.Value value,
                                      IRI context) {
        if (context != null) {
            return connector.getValueFactory().createStatement(subject, property, value, context);
        } else {
            return connector.getValueFactory().createStatement(subject, property, value);
        }
    }
}
