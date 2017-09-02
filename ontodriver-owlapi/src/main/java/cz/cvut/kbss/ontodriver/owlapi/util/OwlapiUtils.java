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
package cz.cvut.kbss.ontodriver.owlapi.util;

import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.vocab.OWL2Datatype;

import java.net.URI;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;

/**
 * Utility methods for the OWLAPI driver.
 */
public class OwlapiUtils {

    private static final String DATE_TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS";

    private OwlapiUtils() {
        throw new AssertionError("Can't create instance.");
    }

    /**
     * Creates OWLLiteral from the specified Java instance.
     *
     * @param value       The value to transform
     * @param dataFactory Data factory
     * @param lang        Ontology language
     * @return OWLLiteral representing the value
     * @throws IllegalArgumentException If {@code value} is of unsupported type
     */
    public static OWLLiteral createOWLLiteralFromValue(Object value, OWLDataFactory dataFactory, String lang) {
        Objects.requireNonNull(value);
        if (value instanceof Integer) {
            // Java implementations map int/Integer to xsd:int, because xsd:integer is unbounded, whereas xsd:int is 32-bit signed, same as Java
            return dataFactory.getOWLLiteral(value.toString(), OWL2Datatype.XSD_INT);
        } else if (value instanceof Long) {
            return dataFactory.getOWLLiteral(value.toString(), OWL2Datatype.XSD_LONG);
        } else if (value instanceof Boolean) {
            return dataFactory.getOWLLiteral((Boolean) value);
        } else if (value instanceof Double) {
            return dataFactory.getOWLLiteral((Double) value);
        } else if (value instanceof String) {
            return dataFactory.getOWLLiteral((String) value, lang);
        } else if (value instanceof Date) {
            SimpleDateFormat sdf = new SimpleDateFormat(DATE_TIME_FORMAT);
            return dataFactory.getOWLLiteral(sdf.format(((Date) value)),
                    dataFactory.getOWLDatatype(OWL2Datatype.XSD_DATE_TIME.getIRI()));
        } else if (value.getClass().isEnum()) {
            return dataFactory.getOWLLiteral(value.toString());
        } else {
            throw new IllegalArgumentException("Unsupported value " + value + " of type " + value.getClass());
        }
    }

    /**
     * Transforms OWLLiteral to a plain Java object (boxed primitive or date/time).
     *
     * @param literal The literal to transform
     * @return Transformed value
     * @throws IllegalArgumentException If the literal is of unsupported type
     */
    public static Object owlLiteralToValue(final OWLLiteral literal) {
        if (literal.isRDFPlainLiteral()) {
            return literal.getLiteral();
        } else if (literal.getDatatype().isBuiltIn())
            switch (literal.getDatatype().getBuiltInDatatype()) {
                case XSD_SHORT:
                    return Short.parseShort(literal.getLiteral());
                case XSD_LONG:
                    return Long.parseLong(literal.getLiteral());
                case XSD_INT:
                case XSD_INTEGER:
                    return Integer.parseInt(literal.getLiteral());
                case XSD_DOUBLE:
                case XSD_DECIMAL:
                    return Double.parseDouble(literal.getLiteral());
                case XSD_FLOAT:
                    return Float.parseFloat(literal.getLiteral());
                case XSD_STRING:
                case RDF_XML_LITERAL:
                    return literal.getLiteral();
                case XSD_BOOLEAN:
                    return Boolean.parseBoolean(literal.getLiteral());
                case XSD_ANY_URI:
                    return URI.create(literal.getLiteral());
                case XSD_DATE_TIME_STAMP:
                case XSD_DATE_TIME:
                    try {
                        return new SimpleDateFormat(DATE_TIME_FORMAT).parse(literal.getLiteral());
                    } catch (ParseException e) {
                        throw new IllegalArgumentException(
                                "The date time '" + literal.getLiteral() + "' cannot be parsed.");
                    }
            }

        throw new IllegalArgumentException("Unsupported datatype: " + literal.getDatatype());
    }

    /**
     * Checks whether the specified literal matches to the specified language.
     * <p>
     * If the literal is not a string, it automatically matches. If it is a string, it matches if {@code language} is
     * not specified, it is without language tag or if the language tag matches the specified language.
     *
     * @param literal  Literal to check
     * @param language Expected language, possibly {@code null}
     * @return {@code true} if the literal matches the language, {@code false} otherwise
     */
    public static boolean doesLanguageMatch(OWLLiteral literal, String language) {
        assert literal != null;

        final OWLDatatype datatype = literal.getDatatype();
        if (datatype.isBuiltIn() && datatype.isString() || datatype.isRDFPlainLiteral()) {
            return language == null || literal.getLang().isEmpty() || literal.getLang().equals(language);
        }
        return true;
    }

    /**
     * Gets OWLNamedIndividual for the specified named resource.
     *
     * @param subject     Named resource to transform to individual
     * @param dataFactory OWL data factory
     * @return OWLNamedIndividual
     */
    public static OWLNamedIndividual getIndividual(NamedResource subject, OWLDataFactory dataFactory) {
        return dataFactory.getOWLNamedIndividual(IRI.create(subject.getIdentifier()));
    }

    /**
     * Checks whether the specified value is a valid IRI.
     * <p>
     * Only absolute IRIs are accepted.
     *
     * @param value The value to check
     * @return {@code true} for instances of {@link NamedResource}, {@link URI}, {@link URL} or {@link IRI} and for
     * Strings parseable by {@link URI#create(String)}.
     */
    public static boolean isIndividualIri(Object value) {
        if (value instanceof NamedResource || value instanceof URI || value instanceof URL || value instanceof IRI) {
            return true;
        }
        if (!(value instanceof String)) {
            return false;
        }
        try {
            final IRI iri = IRI.create(value.toString());
            return iri.isAbsolute();
        } catch (IllegalArgumentException e) {
            return false;
        }
    }
}
