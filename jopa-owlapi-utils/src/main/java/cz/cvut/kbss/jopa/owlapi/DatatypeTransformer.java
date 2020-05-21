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

import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.vocab.OWL2Datatype;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.EnumMap;
import java.util.Map;

public class DatatypeTransformer {

    private static final String DATE_TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS";

    private static final Map<OWL2Datatype, Class<?>> map = new EnumMap<>(OWL2Datatype.class);

    static {
        map.put(OWL2Datatype.RDF_PLAIN_LITERAL, String.class);
        map.put(OWL2Datatype.XSD_STRING, String.class);
        map.put(OWL2Datatype.RDF_XML_LITERAL, String.class);
        map.put(OWL2Datatype.XSD_INT, Integer.class);
        map.put(OWL2Datatype.XSD_INTEGER, BigInteger.class);
        map.put(OWL2Datatype.XSD_DOUBLE, Double.class);
        map.put(OWL2Datatype.XSD_FLOAT, Float.class);
        map.put(OWL2Datatype.XSD_BOOLEAN, Boolean.class);
        map.put(OWL2Datatype.XSD_DATE_TIME, Date.class);
        map.put(OWL2Datatype.XSD_DATE_TIME_STAMP, Date.class);
        map.put(OWL2Datatype.XSD_SHORT, Short.class);
        map.put(OWL2Datatype.XSD_LONG, Long.class);
        map.put(OWL2Datatype.XSD_ANY_URI, URI.class);
        map.put(OWL2Datatype.XSD_DECIMAL, BigDecimal.class);
    }

    private DatatypeTransformer() {
        throw new AssertionError();
    }

    public static Class<?> transformOWLType(final OWLDatatype dt) {
        Class<?> type = null;

        if (dt.isBuiltIn()) {
            type = map.get(dt.getBuiltInDatatype());
        }

        if (type == null) {
            throw new IllegalArgumentException("Unsupported datatype: " + dt);
        }

        return type;
    }

    public static boolean isSupportedJavaType(Class<?> dt) {
        return map.containsValue(dt);
    }

    public static Object transform(final OWLLiteral literal) {
        if (literal.isRDFPlainLiteral()) {
            return literal.getLiteral();
        } else if (literal.getDatatype().isBuiltIn()) {
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
                case RDF_LANG_STRING:
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
                                "The date time '" + literal.getLiteral() + "' cannot be parsed using format " +
                                        DATE_TIME_FORMAT + ".");
                    }
                default:
                    break;
            }
        }
        throw new IllegalArgumentException("Unsupported datatype: " + literal.getDatatype());
    }
}
