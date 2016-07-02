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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;

import java.net.URI;
import java.util.*;

class AxiomSaver {

    private final Connector connector;
    private final ValueFactory valueFactory;
    private final SesameValueConverter valueConverter;

    AxiomSaver(Connector connector, ValueFactory valueFactory, String language) {
        this.connector = connector;
        this.valueFactory = valueFactory;
        this.valueConverter = new SesameValueConverter(valueFactory, language);
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

    void persistAxioms(NamedResource subject, Map<Assertion, Set<Value<?>>> values, URI context) throws SesameDriverException {
        final List<Statement> statements = new ArrayList<>();
        for (Map.Entry<Assertion, Set<Value<?>>> entry : values.entrySet()) {
            statements.addAll(createSesameStatements(subject, entry.getKey(), entry.getValue(), context));
        }
        if (!statements.isEmpty()) {
            connector.addStatements(statements);
        }
    }

    private Collection<? extends Statement> createSesameStatements(NamedResource subject,
                                                                   Assertion assertion, Collection<Value<?>> assertionValues, URI assertionContext)
            throws SesameDriverException {
        final List<Statement> statements = new ArrayList<>(assertionValues.size());

        final org.openrdf.model.Resource subjectUri = SesameUtils.toSesameUri(
                subject.getIdentifier(), valueFactory);
        final org.openrdf.model.URI property = SesameUtils.toSesameUri(assertion.getIdentifier(),
                valueFactory);
        final org.openrdf.model.URI context = assertionContext != null ? SesameUtils.toSesameUri(
                assertionContext, valueFactory) : null;
        for (Value<?> val : assertionValues) {
            if (val == Value.nullValue()) {
                continue;
            }
            org.openrdf.model.Value value = valueConverter.toSesameValue(assertion, val);
            statements.add(createStatement(subjectUri, property, value, context));
        }
        return statements;
    }

    private Statement createStatement(Resource subject, org.openrdf.model.URI property,
                                      org.openrdf.model.Value value, org.openrdf.model.URI context) {
        if (context != null) {
            return valueFactory.createStatement(subject, property, value, context);
        } else {
            return valueFactory.createStatement(subject, property, value);
        }
    }
}
