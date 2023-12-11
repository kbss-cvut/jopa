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
package cz.cvut.kbss.ontodriver.rdf4j.util;

import cz.cvut.kbss.ontodriver.model.*;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Statement;

import java.util.Map;
import java.util.Optional;

public class AxiomBuilder {

    private final NamedResource subject;
    private final Map<IRI, Assertion> propertyToAssertion;

    private final Assertion unspecifiedProperty;

    public AxiomBuilder(NamedResource subject, Map<IRI, Assertion> propertyToAssertion, Assertion unspecifiedProperty) {
        this.subject = subject;
        this.propertyToAssertion = propertyToAssertion;
        this.unspecifiedProperty = unspecifiedProperty;
    }

    public Axiom<?> statementToAxiom(Statement statement) {
        final Assertion assertion = resolveAssertion(statement.getPredicate());

        return statementToAxiom(statement, assertion);
    }

    private static Optional<Value<?>> resolveValue(Statement stmt, Assertion assertion) {
        if (assertion == null || Rdf4jUtils.isBlankNode(stmt.getObject())) {
            return Optional.empty();
        }
        return createValue(assertion, stmt.getObject());
    }

    public Axiom<?> statementToAxiom(Statement statement, Assertion assertion) {
        final Optional<Value<?>> val = resolveValue(statement, assertion);
        return val.map(v -> new AxiomImpl<>(subject, assertion, v)).orElse(null);
    }

    private Assertion resolveAssertion(IRI predicate) {
        Assertion assertion = propertyToAssertion.get(predicate);
        if (assertion == null) {
            if (unspecifiedProperty != null) {
                assertion = Assertion
                        .createPropertyAssertion(Rdf4jUtils.toJavaUri(predicate), unspecifiedProperty.isInferred());
            }
        } else if (assertion.getType() == Assertion.AssertionType.PROPERTY) {
            // If the property was unspecified, create assertion based on the actual property URI
            assertion = Assertion.createPropertyAssertion(Rdf4jUtils.toJavaUri(predicate), assertion.isInferred());
        }
        return assertion;
    }

    private static Optional<Value<?>> createValue(Assertion assertion, org.eclipse.rdf4j.model.Value value) {
        return ValueConverter.fromRdf4jValue(assertion, value).map(Value::new);
    }
}
