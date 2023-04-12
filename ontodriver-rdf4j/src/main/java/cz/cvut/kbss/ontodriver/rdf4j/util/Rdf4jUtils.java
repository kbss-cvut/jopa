/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.util;

import cz.cvut.kbss.jopa.datatype.xsd.XsdDatatypeMapper;
import cz.cvut.kbss.jopa.datatype.xsd.XsdTemporalMapper;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.util.IdentifierUtils;
import org.eclipse.rdf4j.model.*;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.XSD;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalAmount;
import java.util.Date;

/**
 * Utility methods for the RDF4J driver.
 */
public final class Rdf4jUtils {

    private Rdf4jUtils() {
        // Private constructor
    }

    /**
     * Gets value of the specified literal as the corresponding Java object.
     * <p>
     * Primitives are returned boxed. If the type cannot be mapped to a corresponding Java type,
     * it is returned as {@link cz.cvut.kbss.ontodriver.model.Literal}.
     *
     * @param literal RDF literal value
     * @return Java value corresponding to datatype
     */
    public static Object getLiteralValue(Literal literal) {
        assert literal != null;

        final IRI datatype = literal.getDatatype();
        assert datatype != null;

        if (datatype.equals(RDF.LANGSTRING)) {
            return new LangString(literal.stringValue(), literal.getLanguage().orElse(null));
        } else {
            final cz.cvut.kbss.ontodriver.model.Literal lit = cz.cvut.kbss.ontodriver.model.Literal.from(
                    literal.getLabel(), datatype.stringValue());
            return XsdDatatypeMapper.getInstance().map(lit).orElse(lit);
        }
    }

    /**
     * Checks whether the language of the specified literal matches the specified assertion language.
     * <p>
     * If the assertion does not specify a language, any literal will match. If the literal is not a string, it
     * matches as well.
     *
     * @param literal   Literal to check
     * @param assertion Assertion
     * @return {@code false} if the literal is a string literal and its language does not match the one specified by the
     * assertion, {@code true} otherwise
     */
    public static boolean doesLanguageMatch(Literal literal, Assertion assertion) {
        assert assertion != null;
        if (!assertion.hasLanguage()) {
            return true;
        }
        final String language = assertion.getLanguage();
        final IRI datatype = literal.getDatatype();
        if (datatype.equals(XSD.STRING) || datatype.equals(XSD.NORMALIZEDSTRING) ||
                datatype.equals(RDF.LANGSTRING)) {
            return language == null || !literal.getLanguage().isPresent() ||
                    literal.getLanguage().get().equals(language);
        }
        return true;
    }

    /**
     * Creates RDF4J literal from the specified value.
     *
     * @param value    The value to transform
     * @param language Language to add to string literals, optional
     * @param vf       RDF4J value factory
     * @return RFD4J Literal
     * @throws IllegalArgumentException If the type of the value is not supported
     */
    public static Literal createLiteral(Object value, String language, ValueFactory vf) {
        assert value != null;

        if (value instanceof Integer) {
            return vf.createLiteral((Integer) value);
        } else if (value instanceof String) {
            return language != null ? vf.createLiteral((String) value, language) : vf.createLiteral((String) value);
        } else if (value instanceof LangString) {
            final LangString ls = (LangString) value;
            return ls.getLanguage().isPresent() ? vf.createLiteral(ls.getValue(), ls.getLanguage().get()) :
                   vf.createLiteral(ls.getValue());
        } else if (value instanceof Byte) {
            return vf.createLiteral((Byte) value);
        } else if (value instanceof Short) {
            return vf.createLiteral((Short) value);
        } else if (value instanceof Boolean) {
            return vf.createLiteral((Boolean) value);
        } else if (value instanceof Float) {
            return vf.createLiteral((Float) value);
        } else if (value instanceof Double) {
            return vf.createLiteral((Double) value);
        } else if (value instanceof Long) {
            return vf.createLiteral((Long) value);
        } else if (value instanceof BigInteger) {
            return vf.createLiteral((BigInteger) value);
        } else if (value instanceof BigDecimal) {
            return vf.createLiteral((BigDecimal) value);
        } else if (value instanceof Date) {
            final cz.cvut.kbss.ontodriver.model.Literal ontoLiteral = XsdTemporalMapper.map(((Date) value).toInstant());
            return createLiteral(vf, ontoLiteral);
        } else if (value instanceof TemporalAccessor) {
            final cz.cvut.kbss.ontodriver.model.Literal ontoLiteral = XsdTemporalMapper.map((TemporalAccessor) value);
            return createLiteral(vf, ontoLiteral);
        } else if (value instanceof TemporalAmount) {
            final cz.cvut.kbss.ontodriver.model.Literal ontoLiteral = XsdTemporalMapper.map((TemporalAmount) value);
            return createLiteral(vf, ontoLiteral);
        } else if (value.getClass().isEnum()) {
            return vf.createLiteral(value.toString());
        } else if (value instanceof cz.cvut.kbss.ontodriver.model.Literal) {
            final cz.cvut.kbss.ontodriver.model.Literal ontoLiteral = (cz.cvut.kbss.ontodriver.model.Literal) value;
            return createLiteral(vf, ontoLiteral);
        } else {
            throw new IllegalArgumentException("Unsupported literal type " + value.getClass());
        }
    }

    private static Literal createLiteral(ValueFactory vf, cz.cvut.kbss.ontodriver.model.Literal ontoLiteral) {
        return vf.createLiteral(ontoLiteral.getLexicalForm(), vf.createIRI(ontoLiteral.getDatatype()));
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
        return IdentifierUtils.isResourceIdentifier(value);
    }

    /**
     * Constructs a RDF4J IRI from the specified java.net.URI.
     *
     * @param javaUri The uri to convert
     * @param factory RDF4J value factory used for the conversion
     * @return RDF4J IRI
     */
    public static IRI toRdf4jIri(java.net.URI javaUri, ValueFactory factory) {
        return (javaUri != null ? factory.createIRI(javaUri.toString()) : null);
    }

    /**
     * Constructs a RDF4J IRI from identifier of the specified resource.
     * @param resource Resource whose identifier to transform
     * @param factory RDF4J value factory used for the conversion
     * @return RDF4J IRI
     * @see #toRdf4jIri(URI, ValueFactory)
     */
    public static IRI toRdf4jIri(NamedResource resource, ValueFactory factory) {
        return toRdf4jIri(resource.getIdentifier(), factory);
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
            LoggerFactory.getLogger(Rdf4jUtils.class).error("RDF4J resource is not a valid URI: " + e);
            return null;
        }
    }
}
