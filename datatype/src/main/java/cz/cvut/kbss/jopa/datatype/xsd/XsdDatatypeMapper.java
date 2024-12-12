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
package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.DatatypeMapper;
import cz.cvut.kbss.jopa.datatype.exception.DatatypeMappingException;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.util.Objects;
import java.util.Optional;

/**
 * Maps XML Schema types to Java.
 * <p>
 * The mapping logic is based on the known <a href="https://docs.oracle.com/javase/tutorial/jaxb/intro/bind.html">JAXB</a>/
 * <a href="https://xmlbeans.apache.org/docs/2.0.0/guide/conXMLBeansSupportBuiltInSchemaTypes.html">Apache XML
 * Beans</a>
 * mapping with utilization of the Java 8 Date/Time API.
 */
public class XsdDatatypeMapper implements DatatypeMapper {

    public static final String NEGATIVE_INFINITY = "-INF";

    public static final String POSITIVE_INFINITY = "INF";

    private static final XsdDatatypeMapper INSTANCE = new XsdDatatypeMapper();

    /**
     * Gets an instance of this mapper.
     * <p>
     * Convenience method returning one shared instance (the mapper has no state and is thread-safe).
     *
     * @return Shared mapper instance
     */
    public static XsdDatatypeMapper getInstance() {
        return INSTANCE;
    }

    @Override
    public Optional<Object> map(Literal literal) {
        Objects.requireNonNull(literal);
        final String value = literal.getLexicalForm();
        try {
            return switch (literal.getDatatype()) {
                case XSD.BOOLEAN -> Optional.of(Boolean.parseBoolean(value));
                case XSD.BYTE -> Optional.of(Byte.parseByte(value));
                case XSD.SHORT, XSD.UNSIGNED_BYTE -> Optional.of(Short.parseShort(value));
                case XSD.INT, XSD.UNSIGNED_SHORT -> Optional.of(Integer.parseInt(value));
                case XSD.LONG, XSD.UNSIGNED_INT -> Optional.of(Long.parseLong(value));
                case XSD.FLOAT -> Optional.of(toFloat(value));
                case XSD.DOUBLE -> Optional.of(toDouble(value));
                case XSD.STRING, XSD.NORMALIZED_STRING -> Optional.of(value);
                case XSD.DATETIME -> Optional.of(XsdDateTimeMapper.map(value));
                case XSD.DATE -> Optional.of(XsdDateMapper.map(value));
                case XSD.TIME -> Optional.of(XsdTimeMapper.map(value));
                case XSD.DURATION -> Optional.of(XsdDurationMapper.map(value));
                case XSD.INTEGER, XSD.NON_NEGATIVE_INTEGER, XSD.NON_POSITIVE_INTEGER, XSD.NEGATIVE_INTEGER,
                     XSD.POSITIVE_INTEGER, XSD.UNSIGNED_LONG -> Optional.of(new BigInteger(value));
                case XSD.DECIMAL -> Optional.of(new BigDecimal(value));
                case XSD.ANY_URI -> Optional.of(URI.create(value));
                default -> Optional.empty();
            };
        } catch (IllegalArgumentException e) {
            throw new DatatypeMappingException("Unable to map literal " + literal, e);
        }
    }

    private static Float toFloat(String lexicalForm) {
        if (NEGATIVE_INFINITY.equals(lexicalForm)) {
            return Float.NEGATIVE_INFINITY;
        } else if (POSITIVE_INFINITY.equals(lexicalForm)) {
            return Float.POSITIVE_INFINITY;
        }
        return Float.parseFloat(lexicalForm);
    }

    private static Double toDouble(String lexicalForm) {
        if (NEGATIVE_INFINITY.equals(lexicalForm)) {
            return Double.NEGATIVE_INFINITY;
        } else if (POSITIVE_INFINITY.equals(lexicalForm)) {
            return Double.POSITIVE_INFINITY;
        }
        return Double.parseDouble(lexicalForm);
    }
}
