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

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.rdf4j.config.Constants;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;

class ValueConverter {

    private final ValueFactory vf;

    ValueConverter(ValueFactory vf) {
        this.vf = vf;
    }

    Value toRdf4jValue(Assertion assertion, cz.cvut.kbss.ontodriver.model.Value<?> val) throws Rdf4jDriverException {
        switch (assertion.getType()) {
            case DATA_PROPERTY:
                return Rdf4jUtils.createLiteral(val.getValue(), language(assertion), vf);
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

    private IRI getValueAsIri(cz.cvut.kbss.ontodriver.model.Value<?> val) throws Rdf4jDriverException {
        try {
            return vf.createIRI(val.getValue().toString());
        } catch (IllegalArgumentException e) {
            throw new Rdf4jDriverException(e);
        }
    }

    private Value resolvePropertyValue(Assertion assertion, cz.cvut.kbss.ontodriver.model.Value<?> val) {
        final Object value = val.getValue();
        if (!(value instanceof String && !assertion.hasLanguage()) && Rdf4jUtils.isResourceIdentifier(value)) {
            return vf.createIRI(value.toString());
        } else {
            return Rdf4jUtils.createLiteral(value, language(assertion), vf);
        }
    }
}
