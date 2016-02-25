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

import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;

class SesameValueConverter {

    private ValueFactory vf;
    private String language;

    SesameValueConverter(ValueFactory vf, String language) {
        this.vf = vf;
        this.language = language;
    }

    Value toSesameValue(Assertion assertion, cz.cvut.kbss.ontodriver.model.Value<?> val)
            throws SesameDriverException {
        switch (assertion.getType()) {
            case ANNOTATION_PROPERTY:
            case DATA_PROPERTY:
                return SesameUtils.createDataPropertyLiteral(val.getValue(), language, vf);
            case CLASS:
            case OBJECT_PROPERTY:
                return getValueAsSesameUri(val);
            case PROPERTY:
                return resolvePropertyValue(val);
            default:
                // Failsafe
                throw new IllegalArgumentException("Unsupported assertion type " + assertion.getType());
        }
    }

    private org.openrdf.model.URI getValueAsSesameUri(cz.cvut.kbss.ontodriver.model.Value<?> val) throws SesameDriverException {
        try {
            return vf.createURI(val.getValue().toString());
        } catch (IllegalArgumentException e) {
            throw new SesameDriverException(e);
        }
    }

    private org.openrdf.model.Value resolvePropertyValue(cz.cvut.kbss.ontodriver.model.Value<?> val) {
        try {
            return getValueAsSesameUri(val);
        } catch (SesameDriverException e) {
            return SesameUtils.createDataPropertyLiteral(val.getValue(), language, vf);
        }
    }
}