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
package cz.cvut.kbss.ontodriver.sesame.util;

import cz.cvut.kbss.ontodriver.model.*;
import org.openrdf.model.Literal;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;

import java.util.Map;

public class AxiomBuilder {

    private final NamedResource subject;
    private final Map<URI, Assertion> propertyToAssertion;

    private final Assertion unspecifiedProperty;

    public AxiomBuilder(NamedResource subject, Map<URI, Assertion> propertyToAssertion, Assertion unspecifiedProperty) {
        this.subject = subject;
        this.propertyToAssertion = propertyToAssertion;
        this.unspecifiedProperty = unspecifiedProperty;
    }

    public Axiom<?> statementToAxiom(Statement statement) {
        Assertion assertion = resolveAssertion(statement.getPredicate());

        final Value<?> val = resolveValue(statement, assertion);
        if (val == null) {
            return null;
        }
        return new AxiomImpl<>(subject, assertion, val);
    }

    private Value<?> resolveValue(Statement stmt, Assertion assertion) {
        if (assertion == null || SesameUtils.isBlankNode(stmt.getObject())) {
            return null;
        }
        Value<?> val = createValue(assertion.getType(), stmt.getObject());
        if (val == null) {
            return null;
        }
        return val;
    }

    public Axiom<?> statementToAxiom(Statement statement, Assertion assertion) {
        final Value<?> val = resolveValue(statement, assertion);
        if (val == null) {
            return null;
        }
        return new AxiomImpl<>(subject, assertion, val);
    }

    private Assertion resolveAssertion(URI predicate) {
        Assertion assertion = propertyToAssertion.get(predicate);
        if (assertion == null) {
            if (unspecifiedProperty != null) {
                assertion = Assertion
                        .createPropertyAssertion(SesameUtils.toJavaUri(predicate), unspecifiedProperty.isInferred());
            } else {
                assertion = null;
            }
        } else if (assertion.getType() == Assertion.AssertionType.PROPERTY) {
            // If the property was unspecified, create assertion based on the actual property URI
            assertion = Assertion.createPropertyAssertion(SesameUtils.toJavaUri(predicate), assertion.isInferred());
        }
        return assertion;
    }

    private Value<?> createValue(Assertion.AssertionType assertionType, org.openrdf.model.Value value) {
        switch (assertionType) {
            case DATA_PROPERTY:
                if (!(value instanceof Literal)) {
                    return null;
                }
                return new Value<>(SesameUtils.getDataPropertyValue((Literal) value));
            case CLASS:
                if (!(value instanceof Resource)) {
                    return null;
                }
                return new Value<>(SesameUtils.toJavaUri((Resource) value));
            case OBJECT_PROPERTY:
                if (!(value instanceof Resource)) {
                    return null;
                }
                return new Value<>(NamedResource.create(value.stringValue()));
            case ANNOTATION_PROPERTY:   // Intentional fall-through
            case PROPERTY:
                return resolveValue(value);
        }
        return null;
    }

    private Value<?> resolveValue(org.openrdf.model.Value object) {
        if (object instanceof Literal) {
            return new Value<>(SesameUtils.getDataPropertyValue((Literal) object));
        } else {
            return new Value<>(SesameUtils.toJavaUri((Resource) object));
        }
    }
}
