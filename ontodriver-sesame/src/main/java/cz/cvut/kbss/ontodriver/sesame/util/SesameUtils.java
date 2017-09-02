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

import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.eclipse.rdf4j.model.*;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.XMLSchema;
import org.slf4j.LoggerFactory;

import java.net.URL;
import java.util.Date;

/**
 * Utility methods for the Sesame driver.
 */
public final class SesameUtils {

    private SesameUtils() {
        // Private constructor
    }

    /**
     * Gets value of the specified data property literal as the corresponding Java object.
     * <p>
     * Primitives are returned boxed.
     *
     * @param literal DataProperty value
     * @return Java value corresponding to the XML Schema datatype
     * @throws IllegalArgumentException If literal's datatype is not supported
     */
    public static Object getDataPropertyValue(Literal literal) {
        assert literal != null;

        final IRI datatype = literal.getDatatype();
        assert datatype != null;

        if (datatype.equals(XMLSchema.STRING) || datatype.equals(XMLSchema.NORMALIZEDSTRING) ||
                datatype.equals(RDF.LANGSTRING)) {
            return literal.stringValue();
        } else if (datatype.equals(XMLSchema.INT) || datatype.equals(XMLSchema.UNSIGNED_INT)) {
            return literal.intValue();
        } else if (datatype.equals(XMLSchema.INTEGER)
                || datatype.equals(XMLSchema.POSITIVE_INTEGER)
                || datatype.equals(XMLSchema.NON_NEGATIVE_INTEGER)
                || datatype.equals(XMLSchema.NEGATIVE_INTEGER)
                || datatype.equals(XMLSchema.NON_POSITIVE_INTEGER)) {
            return literal.intValue();
        } else if (datatype.equals(XMLSchema.BOOLEAN)) {
            return literal.booleanValue();
        } else if (datatype.equals(XMLSchema.LONG) || datatype.equals(XMLSchema.UNSIGNED_LONG)) {
            return literal.longValue();
        } else if (datatype.equals(XMLSchema.DECIMAL)) {
            return literal.decimalValue();
        } else if (datatype.equals(XMLSchema.DOUBLE)) {
            return literal.doubleValue();
        } else if (datatype.equals(XMLSchema.SHORT) || datatype.equals(XMLSchema.UNSIGNED_SHORT)) {
            return literal.shortValue();
        } else if (datatype.equals(XMLSchema.BYTE) || datatype.equals(XMLSchema.UNSIGNED_BYTE)) {
            return literal.byteValue();
        } else if (datatype.equals(XMLSchema.DATE) || datatype.equals(XMLSchema.DATETIME)) {
            return literal.calendarValue().toGregorianCalendar().getTime();
        } else {
            throw new IllegalArgumentException("Unsupported datatype " + datatype);
        }
    }

    /**
     * Checks whether the language of the specified string literal matches the expected one.
     * <p>
     * If the literal is not a string, it always matches.
     *
     * @param literal  Literal to check
     * @param language Expected language, can be {@code null}
     * @return {@code false} if the literal is a string literal and its language does not match the expected one, {@code
     * true} otherwise
     */
    public static boolean doesLanguageMatch(Literal literal, String language) {
        assert literal != null;
        final IRI datatype = literal.getDatatype();
        if (datatype.equals(XMLSchema.STRING) || datatype.equals(XMLSchema.NORMALIZEDSTRING) ||
                datatype.equals(RDF.LANGSTRING)) {
            return language == null || !literal.getLanguage().isPresent() ||
                    literal.getLanguage().get().equals(language);
        }
        return true;
    }

    /**
     * Creates Sesame literal from the specified value, which can be used as data property object.
     *
     * @param value    The value to transform
     * @param language Language to add to string literals, optional
     * @param vf       Sesame value factory
     * @return Sesame Literal
     * @throws IllegalArgumentException If the type of the value is not supported
     */
    public static Literal createDataPropertyLiteral(Object value, String language, ValueFactory vf) {
        assert value != null;

        if (value instanceof Integer) {
            return vf.createLiteral((Integer) value);
        } else if (value instanceof String) {
            return language != null ? vf.createLiteral((String) value, language) : vf.createLiteral((String) value);
        } else if (value instanceof Byte) {
            return vf.createLiteral((Byte) value);
        } else if (value instanceof Short) {
            return vf.createLiteral((Short) value);
        } else if (value instanceof Boolean) {
            return vf.createLiteral((Boolean) value);
        } else if (value instanceof Double) {
            return vf.createLiteral((Double) value);
        } else if (value instanceof Long) {
            return vf.createLiteral((Long) value);
        } else if (value instanceof Date) {
            return vf.createLiteral((Date) value);
        } else if (value.getClass().isEnum()) {
            return vf.createLiteral(value.toString());
        } else {
            throw new IllegalArgumentException("Unsupported literal type " + value.getClass());
        }
    }

    /**
     * Checks whether the specified value is a blank node.
     *
     * @param value The value to check
     * @return {@code true} if the value is a blank node, {@code false} otherwise
     */
    public static boolean isBlankNode(Value value) {
        assert value != null;
        return value instanceof BNode;
    }

    /**
     * Resolves whether the specified value is a resource identifier.
     * <p>
     * Only absolute IRIs are supported (i.e. no blank node identifiers).
     *
     * @param value The value to check
     * @return {@code true} if the value is either an URI or an URL
     */
    public static boolean isResourceIdentifier(Object value) {
        if (value instanceof NamedResource || value instanceof java.net.URI || value instanceof URL) {
            return true;
        }
        if (!(value instanceof String)) {
            return false;
        }
        try {
            final java.net.URI uri = java.net.URI.create(value.toString());
            return uri.isAbsolute();
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    /**
     * Constructs a Sesame URI from the specified java.net.URI.
     *
     * @param javaUri The uri to convert
     * @param factory ValueFactory used for the conversion
     * @return Sesame IRI
     */
    public static IRI toSesameIri(java.net.URI javaUri, ValueFactory factory) {
        return (javaUri != null ? factory.createIRI(javaUri.toString()) : null);
    }

    public static java.net.URI toJavaUri(Resource resource) {
        if (resource instanceof BNode) {
            // We have to check for BNode explicitly, because java's URI treats
            // BNode's identifier as a valid URI
            return null;
        }
        try {
            return java.net.URI.create(resource.stringValue());
        } catch (IllegalArgumentException e) {
            // This shouldn't happen
            LoggerFactory.getLogger(SesameUtils.class).error("Sesame resource is not a valid URI: " + e);
            return null;
        }
    }
}
