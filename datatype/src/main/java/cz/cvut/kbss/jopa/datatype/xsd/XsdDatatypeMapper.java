/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
 * <a href="https://xmlbeans.apache.org/docs/2.0.0/guide/conXMLBeansSupportBuiltInSchemaTypes.html">Apache XML Beans</a>
 * mapping with utilization of the Java 8 Date/Time API.
 */
public class XsdDatatypeMapper implements DatatypeMapper {

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
            switch (literal.getDatatype()) {
                case XSD.BOOLEAN:
                    return Optional.of(Boolean.parseBoolean(value));
                case XSD.BYTE:
                    return Optional.of(Byte.parseByte(value));
                case XSD.SHORT:
                case XSD.UNSIGNED_BYTE:
                    return Optional.of(Short.parseShort(value));
                case XSD.INT:
                case XSD.UNSIGNED_SHORT:
                    return Optional.of(Integer.parseInt(value));
                case XSD.LONG:
                case XSD.UNSIGNED_INT:
                    return Optional.of(Long.parseLong(value));
                case XSD.FLOAT:
                    return Optional.of(Float.parseFloat(value));
                case XSD.DOUBLE:
                    return Optional.of(Double.parseDouble(value));
                case XSD.STRING:
                case XSD.NORMALIZED_STRING:
                    return Optional.of(value);
                case XSD.DATETIME:
                    return Optional.of(XsdDateTimeMapper.map(value));
                case XSD.DATE:
                    return Optional.of(XsdDateMapper.map(value));
                case XSD.TIME:
                    return Optional.of(XsdTimeMapper.map(value));
                case XSD.DURATION:
                    return Optional.of(XsdDurationMapper.map(value));
                case XSD.INTEGER:
                case XSD.NON_NEGATIVE_INTEGER:
                case XSD.NON_POSITIVE_INTEGER:
                case XSD.NEGATIVE_INTEGER:
                case XSD.POSITIVE_INTEGER:
                case XSD.UNSIGNED_LONG:
                    return Optional.of(new BigInteger(value));
                case XSD.DECIMAL:
                    return Optional.of(new BigDecimal(value));
                case XSD.ANY_URI:
                    return Optional.of(URI.create(value));
                default:
                    return Optional.empty();
            }
        } catch (IllegalArgumentException e) {
            throw new DatatypeMappingException("Unable to map literal " + literal, e);
        }
    }
}
