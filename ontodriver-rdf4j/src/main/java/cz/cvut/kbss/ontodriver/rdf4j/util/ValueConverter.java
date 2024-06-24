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
package cz.cvut.kbss.ontodriver.rdf4j.util;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.config.Constants;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;

import java.util.Optional;

/**
 * Converts values between the OntoDriver model and RDF4J model.
 */
public class ValueConverter {

    private final ValueFactory vf;

    public ValueConverter(ValueFactory vf) {
        this.vf = vf;
    }

    public static Optional<Object> fromRdf4jValue(Assertion assertion, Value value) {
        final Assertion.AssertionType assertionType = assertion.getType();
        switch (assertionType) {
            case DATA_PROPERTY:
                if (!(value instanceof Literal) || !Rdf4jUtils.doesLanguageMatch((Literal) value, assertion)) {
                    return Optional.empty();
                }
                return Optional.of(Rdf4jUtils.getLiteralValue((Literal) value));
            case CLASS:
                if (!(value instanceof Resource)) {
                    return Optional.empty();
                }
                return Optional.ofNullable(Rdf4jUtils.toJavaUri((Resource) value));
            case OBJECT_PROPERTY:
                if (!(value instanceof Resource)) {
                    return Optional.empty();
                }
                return Optional.of(NamedResource.create(value.stringValue()));
            case ANNOTATION_PROPERTY:   // Intentional fall-through
            case PROPERTY:
                return resolveUnknownPropertyTypeValue(assertion, value);
        }
        return Optional.empty();
    }

    private static Optional<Object> resolveUnknownPropertyTypeValue(Assertion assertion, Value value) {
        if (value instanceof Literal) {
            if (!Rdf4jUtils.doesLanguageMatch((Literal) value, assertion)) {
                return Optional.empty();
            }
            return Optional.of(Rdf4jUtils.getLiteralValue((Literal) value));
        } else {
            return Optional.of(NamedResource.create(value.stringValue()));
        }
    }

    public Value toRdf4jValue(Assertion assertion, cz.cvut.kbss.ontodriver.model.Value<?> val) throws Rdf4jDriverException {
        return toRdf4jValue(assertion, val.getValue());
    }

    public Value toRdf4jValue(Assertion assertion, Object val) throws Rdf4jDriverException {
        switch (assertion.getType()) {
            case DATA_PROPERTY:
                return Rdf4jUtils.createLiteral(val, language(assertion), vf);
            case CLASS:
            case OBJECT_PROPERTY:
                return getValueAsIri(val);
            case ANNOTATION_PROPERTY:   // Intentional fall-through
            case PROPERTY:
                return resolvePropertyValue(assertion, val);
            default:
                // Failsafe
                throw new IllegalArgumentException("Unsupported assertion type " + assertion.getType());
        }
    }

    private static String language(Assertion assertion) {
        return assertion.hasLanguage() ? assertion.getLanguage() : Constants.DEFAULT_LANG;
    }

    private IRI getValueAsIri(Object val) throws Rdf4jDriverException {
        try {
            return vf.createIRI(val.toString());
        } catch (IllegalArgumentException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    private Value resolvePropertyValue(Assertion assertion, Object val) {
        if (!(val instanceof String && !assertion.hasLanguage()) && Rdf4jUtils.isResourceIdentifier(val)) {
            return vf.createIRI(val.toString());
        } else {
            return Rdf4jUtils.createLiteral(val, language(assertion), vf);
        }
    }
}
