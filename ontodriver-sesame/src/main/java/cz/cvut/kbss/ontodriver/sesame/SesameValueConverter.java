/**
 * Copyright (C) 2019 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;

class SesameValueConverter {

    private ValueFactory vf;
    private String language;

    SesameValueConverter(ValueFactory vf, String language) {
        this.vf = vf;
        this.language = language;
    }

    Value toSesameValue(Assertion assertion, cz.cvut.kbss.ontodriver.model.Value<?> val) throws SesameDriverException {
        switch (assertion.getType()) {
            case DATA_PROPERTY:
                return SesameUtils.createDataPropertyLiteral(val.getValue(), language(assertion), vf);
            case CLASS:
            case OBJECT_PROPERTY:
                return getValueAsSesameUri(val);
            case ANNOTATION_PROPERTY:   // Intentional fall-through
            case PROPERTY:
                return resolvePropertyValue(assertion, val);
            default:
                // Failsafe
                throw new IllegalArgumentException("Unsupported assertion type " + assertion.getType());
        }
    }

    private String language(Assertion assertion) {
        return assertion.hasLanguage() ? assertion.getLanguage() : language;
    }

    private IRI getValueAsSesameUri(cz.cvut.kbss.ontodriver.model.Value<?> val) throws SesameDriverException {
        try {
            return vf.createIRI(val.getValue().toString());
        } catch (IllegalArgumentException e) {
            throw new SesameDriverException(e);
        }
    }

    private Value resolvePropertyValue(Assertion assertion, cz.cvut.kbss.ontodriver.model.Value<?> val) {
        if (SesameUtils.isResourceIdentifier(val.getValue())) {
            return vf.createIRI(val.getValue().toString());
        } else {
            return SesameUtils.createDataPropertyLiteral(val.getValue(), language(assertion), vf);
        }
    }
}