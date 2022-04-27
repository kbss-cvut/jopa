/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;

import java.net.URI;
import java.util.*;

class AxiomSaver {

    private final Connector connector;
    private final SesameValueConverter valueConverter;

    AxiomSaver(Connector connector) {
        this.connector = connector;
        this.valueConverter = new SesameValueConverter(connector.getValueFactory());
    }

    void persistAxioms(AxiomValueDescriptor axiomDescriptor) throws SesameDriverException {
        final List<Statement> statements = new ArrayList<>();
        for (Assertion assertion : axiomDescriptor.getAssertions()) {
            statements.addAll(createSesameStatements(axiomDescriptor.getSubject(), assertion,
                    axiomDescriptor.getAssertionValues(assertion),
                    axiomDescriptor.getAssertionContext(assertion)));
        }
        if (!statements.isEmpty()) {
            connector.addStatements(statements);
        }
    }

    void persistAxioms(NamedResource subject, Map<Assertion, Set<Value<?>>> values, URI context)
            throws SesameDriverException {
        final List<Statement> statements = new ArrayList<>();
        for (Map.Entry<Assertion, Set<Value<?>>> entry : values.entrySet()) {
            statements.addAll(createSesameStatements(subject, entry.getKey(), entry.getValue(), context));
        }
        if (!statements.isEmpty()) {
            connector.addStatements(statements);
        }
    }

    private Collection<? extends Statement> createSesameStatements(NamedResource subject,
                                                                   Assertion assertion,
                                                                   Collection<Value<?>> assertionValues,
                                                                   URI assertionContext)
            throws SesameDriverException {
        final List<Statement> statements = new ArrayList<>(assertionValues.size());

        final Resource subjectUri = SesameUtils.toSesameIri(subject.getIdentifier(), connector.getValueFactory());
        final IRI property = SesameUtils.toSesameIri(assertion.getIdentifier(), connector.getValueFactory());
        final IRI context = assertionContext != null ? SesameUtils.toSesameIri(assertionContext, connector.getValueFactory()) : null;
        for (Value<?> val : assertionValues) {
            if (val == Value.nullValue()) {
                continue;
            }
            org.eclipse.rdf4j.model.Value value = valueConverter.toSesameValue(assertion, val);
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
