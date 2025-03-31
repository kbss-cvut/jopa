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
package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.owl2java.exception.UnsupportedICException;
import org.semanticweb.owlapi.model.*;

public class Utils {

    private Utils() {
        throw new AssertionError();
    }

    static OWLDatatype ensureDatatype(final OWLDataRange r) {
        if (!r.isOWLDatatype()) {
            throw new UnsupportedICException("Data ranges not supported: " + r);
        }

        if (!r.asOWLDatatype().isBuiltIn()) {
            throw new UnsupportedICException("Only built in datatypes are supported: " + r);
        }
        return r.asOWLDatatype();
    }

    static OWLClass ensureClass(final OWLClassExpression r) {
        if (!r.isAnonymous()) {
            return r.asOWLClass();
        }
        throw new UnsupportedICException("Only named classes are supported: " + r);
    }

    static OWLDataProperty ensureDataProperty(final OWLDataPropertyExpression e) {
        if (e.isAnonymous()) {
            throw new UnsupportedICException("Data property expressions not supported: " + e);
        }
        return e.asOWLDataProperty();
    }

    static OWLObjectProperty ensureObjectProperty(final OWLObjectPropertyExpression e) {
        if (e.isAnonymous()) {
            throw new UnsupportedICException("Object property expressions not supported: " + e);
        }
        return e.asOWLObjectProperty();
    }
}
