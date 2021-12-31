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
package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.datatype.xsd.XsdTemporalMapper;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.vocab.OWL2Datatype;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.time.OffsetDateTime;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalAmount;
import java.util.Date;
import java.util.EnumMap;
import java.util.Map;
import java.util.Objects;

/**
 * Contains utility functions for transformations between OWL2 datatypes (usually based on XSD types) and Java types.
 * <p>
 * Mostly based on <a href="https://xmlbeans.apache.org/docs/3.0.0/guide/conXMLBeansSupportBuiltInSchemaTypes.html">https://xmlbeans.apache.org/docs/3.0.0/guide/conXMLBeansSupportBuiltInSchemaTypes.html</a>
 */
public class DatatypeTransformer {

    /**
     * Mapping between {@link OWL2Datatype}s and Java types.
     * <p>
     * Note that the map is incomplete (e.g., the {@link OWL2Datatype} enum does not contain constants for {@code xsd:date} and {@code xsd:time}.
     * <p>
     * Also, OWL API maps {@code xsd:integer} to Java {@link Integer}, which is technically not correct, since {@code xsd:integer} is unbound
     * and may not fit, so {@link BigInteger} would be more appropriate. The same goes for other {@code xsd:integer} derivatives like {@code xsd:negativeInteger} etc.
     */
    private static final Map<OWL2Datatype, Class<?>> DATATYPE_MAP = new EnumMap<>(OWL2Datatype.class);

    private static final OWLDataFactory DATA_FACTORY = new OWLDataFactoryImpl();

    static {
        DATATYPE_MAP.put(OWL2Datatype.RDF_PLAIN_LITERAL, String.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_STRING, String.class);
        DATATYPE_MAP.put(OWL2Datatype.RDF_XML_LITERAL, String.class);
        DATATYPE_MAP.put(OWL2Datatype.RDF_LANG_STRING, MultilingualString.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_INT, Integer.class);
        // technically, this is not correct, as XSD integer should be mapped to BigInteger, because it is unbound
        // But OWL API maps it to integer, so we have to keep it this way too
        DATATYPE_MAP.put(OWL2Datatype.XSD_INTEGER, Integer.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_NON_NEGATIVE_INTEGER, Integer.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_POSITIVE_INTEGER, Integer.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_NON_POSITIVE_INTEGER, Integer.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_NEGATIVE_INTEGER, Integer.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_UNSIGNED_INT, Long.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_DOUBLE, Double.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_FLOAT, Float.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_BOOLEAN, Boolean.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_DATE_TIME, OffsetDateTime.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_DATE_TIME_STAMP, OffsetDateTime.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_SHORT, Short.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_UNSIGNED_SHORT, Integer.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_LONG, Long.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_UNSIGNED_LONG, BigInteger.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_BYTE, Byte.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_UNSIGNED_BYTE, Short.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_ANY_URI, URI.class);
        DATATYPE_MAP.put(OWL2Datatype.XSD_DECIMAL, BigDecimal.class);
    }

    private DatatypeTransformer() {
        throw new AssertionError();
    }

    public static Class<?> transformOWLType(final OWLDatatype dt) {
        Class<?> type = null;

        if (dt.isBuiltIn()) {
            type = DATATYPE_MAP.get(dt.getBuiltInDatatype());
        }

        if (type == null) {
            throw new IllegalArgumentException("Unsupported datatype: " + dt);
        }

        return type;
    }

    public static boolean isSupportedJavaType(Class<?> dt) {
        return DATATYPE_MAP.containsValue(dt);
    }

    /**
     * Transforms the specified {@link OWLLiteral} to the corresponding Java type (if possible).
     * <p>
     * For instance, literals of type {@code xsd:int} are transformed to Java {@link Integer}s.
     *
     * @param literal Literal to transform
     * @return Java object corresponding to the literal
     * @deprecated Use datatype mappers from the {@code datatype} module instead
     */
    @Deprecated
    public static Object transform(final OWLLiteral literal) {
        if (literal.isRDFPlainLiteral()) {
            return literal.getLiteral();
        } else if (literal.getDatatype().isBuiltIn()) {
            switch (literal.getDatatype().getBuiltInDatatype()) {
                case XSD_SHORT:
                case XSD_UNSIGNED_BYTE:
                    return Short.parseShort(literal.getLiteral());
                case XSD_LONG:
                case XSD_UNSIGNED_INT:
                    return Long.parseLong(literal.getLiteral());
                case XSD_INT:
                case XSD_INTEGER:
                case XSD_NON_NEGATIVE_INTEGER:
                case XSD_NON_POSITIVE_INTEGER:
                case XSD_POSITIVE_INTEGER:
                case XSD_NEGATIVE_INTEGER:
                case XSD_UNSIGNED_SHORT:
                    return Integer.parseInt(literal.getLiteral());
                case XSD_DOUBLE:
                    return Double.parseDouble(literal.getLiteral());
                case XSD_FLOAT:
                    return Float.parseFloat(literal.getLiteral());
                case XSD_DECIMAL:
                    return new BigDecimal(literal.getLiteral());
                case XSD_STRING:
                case RDF_XML_LITERAL:
                    return literal.getLiteral();
                case RDF_LANG_STRING:
                    return new LangString(literal.getLiteral(), literal.getLang());
                case XSD_BOOLEAN:
                    return Boolean.parseBoolean(literal.getLiteral());
                case XSD_ANY_URI:
                    return URI.create(literal.getLiteral());
                default:
                    break;
            }
        }
        return new Literal(literal.getLiteral(), literal.getDatatype().toStringID());
    }

    /**
     * Transforms the specified Java object to an {@link OWLLiteral}.
     * <p>
     * The datatype is determined from the object's type.
     *
     * @param value Value to transform
     * @param lang  Optional language for string literals
     * @return {@code OWLiteral}
     */
    public static OWLLiteral transform(Object value, String lang) {
        Objects.requireNonNull(value);
        if (value instanceof Integer) {
            // Java implementations map int/Integer to xsd:int, because xsd:integer is unbounded, whereas xsd:int is 32-bit signed, same as Java
            return DATA_FACTORY.getOWLLiteral(value.toString(), OWL2Datatype.XSD_INT);
        } else if (value instanceof BigInteger) {
            return DATA_FACTORY.getOWLLiteral(value.toString(), OWL2Datatype.XSD_INTEGER);
        } else if (value instanceof Long) {
            return DATA_FACTORY.getOWLLiteral(value.toString(), OWL2Datatype.XSD_LONG);
        } else if (value instanceof Boolean) {
            return DATA_FACTORY.getOWLLiteral((Boolean) value);
        } else if (value instanceof Double) {
            return DATA_FACTORY.getOWLLiteral((Double) value);
        } else if (value instanceof Float) {
            return DATA_FACTORY.getOWLLiteral((Float) value);
        } else if (value instanceof BigDecimal) {
            return DATA_FACTORY.getOWLLiteral(((BigDecimal) value).toPlainString(), OWL2Datatype.XSD_DECIMAL);
        } else if (value instanceof LangString) {
            final LangString ls = (LangString) value;
            return DATA_FACTORY.getOWLLiteral(ls.getValue(), ls.getLanguage().orElse(null));
        } else if (value instanceof String) {
            return lang != null ? DATA_FACTORY.getOWLLiteral(value.toString(), lang) :
                    DATA_FACTORY.getOWLLiteral(value.toString());
        } else if (value instanceof Date) {
            final Literal ontoLiteral = XsdTemporalMapper.map(((Date) value).toInstant());
            return toOwlLiteral(ontoLiteral);
        } else if (value instanceof TemporalAccessor) {
            final Literal ontoLiteral = XsdTemporalMapper.map(((TemporalAccessor) value));
            return toOwlLiteral(ontoLiteral);
        } else if (value instanceof TemporalAmount) {
            final Literal ontoLiteral = XsdTemporalMapper.map(((TemporalAmount) value));
            return toOwlLiteral(ontoLiteral);
        } else if (value.getClass().isEnum()) {
            return DATA_FACTORY.getOWLLiteral(value.toString());
        } else if (value instanceof Literal) {
            return toOwlLiteral((Literal) value);
        } else {
            throw new IllegalArgumentException("Unsupported value " + value + " of type " + value.getClass());
        }
    }

    private static OWLLiteral toOwlLiteral(Literal ontoLiteral) {
        return DATA_FACTORY.getOWLLiteral(ontoLiteral.getLexicalForm(),
                DATA_FACTORY.getOWLDatatype(ontoLiteral.getDatatype()));
    }
}
