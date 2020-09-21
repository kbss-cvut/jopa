/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.util.IdentifierUtils;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.datatypes.xsd.impl.RDFLangString;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.ResourceFactory;

/**
 * Utility methods for working with Jena API.
 */
public class JenaUtils {

    private JenaUtils() {
        throw new AssertionError();
    }

    /**
     * Transforms the specified value to an {@link RDFNode}, be it a resource or a literal.
     *
     * @param assertion Assertion representing the asserted property
     * @param value     Value to transform
     * @return Jena RDFNode
     */
    public static RDFNode valueToRdfNode(Assertion assertion, Value<?> value) {
        if (IdentifierUtils.isResourceIdentifier(value.getValue())) {
            return ResourceFactory.createResource(value.stringValue());
        } else {
            return assertion.hasLanguage() ?
                   ResourceFactory.createLangLiteral(value.stringValue(), assertion.getLanguage()) :
                   ResourceFactory.createTypedLiteral(value.getValue());
        }
    }

    public static Object literalToValue(Literal literal) {
        if (literal.getDatatype().equals(RDFLangString.rdfLangString)) {
            return new LangString(literal.getString(), literal.getLanguage());
        }
        // This is because Jena returns XSD:long values as Integers, when they fit. But we don't want this.
        return literal.getDatatype().equals(XSDDatatype.XSDlong) ? literal.getLong() : literal.getValue();
    }
}
