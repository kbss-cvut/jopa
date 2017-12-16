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
package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.owl2java.exception.UnsupportedICException;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyExpression;
import org.semanticweb.owlapi.model.OWLDataRange;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;

public class Utils {

    static OWLDatatype ensureDatatype(final OWLDataRange r)
            throws UnsupportedICException {
        if (!r.isDatatype()) {
            throw new UnsupportedICException("Data ranges not supported: " + r);
        }

        if (!r.asOWLDatatype().isBuiltIn()) {
            throw new UnsupportedICException(
                "Only built in datatypes are supported: " + r);
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
            throw new UnsupportedICException(
                "Data property expressions not supported: " + e);
        }

        return e.asOWLDataProperty();
    }

    static OWLObjectProperty ensureObjectProperty(
        final OWLObjectPropertyExpression e) throws UnsupportedICException {
        if (e.isAnonymous()) {
            throw new UnsupportedICException(
                "Object property expressions not supported: " + e);
        }

        return e.asOWLObjectProperty();
    }
}
