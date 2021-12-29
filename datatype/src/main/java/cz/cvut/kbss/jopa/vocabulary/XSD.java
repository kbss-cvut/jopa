/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.vocabulary;

/**
 * A subset of the XML Schema built-in data types vocabulary.
 */
public final class XSD {

    /**
     * XSD vocabulary namespace.
     */
    public static final String NAMESPACE = "http://www.w3.org/2001/XMLSchema#";

    /**
     * Typical prefix used for {@link #NAMESPACE}.
     */
    public static final String PREFIX = "xsd";

    /**
     * {@code boolean} XML Schema data type.
     */
    public static final String BOOLEAN = NAMESPACE + "boolean";

    /**
     * {@code byte} XML Schema data type.
     */
    public static final String BYTE = NAMESPACE + "byte";

    /**
     * {@code unsignedByte} XML Schema data type.
     */
    public static final String UNSIGNED_BYTE = NAMESPACE + "unsignedByte";

    /**
     * {@code short} XML Schema data type.
     */
    public static final String SHORT = NAMESPACE + "short";

    /**
     * {@code unsignedShort} XML Schema data type.
     */
    public static final String UNSIGNED_SHORT = NAMESPACE + "unsignedShort";

    /**
     * {@code int} XML Schema data type.
     */
    public static final String INT = NAMESPACE + "int";

    /**
     * {@code unsignedInt} XML Schema data type.
     */
    public static final String UNSIGNED_INT = NAMESPACE + "unsignedInt";

    /**
     * {@code integer} XML Schema data type.
     */
    public static final String INTEGER = NAMESPACE + "integer";

    /**
     * {@code nonPositiveInteger} XML Schema data type.
     */
    public static final String NON_POSITIVE_INTEGER = NAMESPACE + "nonPositiveInteger";

    /**
     * {@code negativeInteger} XML Schema data type.
     */
    public static final String NEGATIVE_INTEGER = NAMESPACE + "negativeInteger";

    /**
     * {@code nonNegativeInteger} XML Schema data type.
     */
    public static final String NON_NEGATIVE_INTEGER = NAMESPACE + "nonNegativeInteger";

    /**
     * {@code positiveInteger} XML Schema data type.
     */
    public static final String POSITIVE_INTEGER = NAMESPACE + "positiveInteger";

    /**
     * {@code long} XML Schema data type.
     */
    public static final String LONG = NAMESPACE + "long";

    /**
     * {@code unsignedLong} XML Schema data type.
     */
    public static final String UNSIGNED_LONG = NAMESPACE + "unsignedLong";

    /**
     * {@code double} XML Schema data type.
     */
    public static final String DOUBLE = NAMESPACE + "double";

    /**
     * {@code float} XML Schema data type.
     */
    public static final String FLOAT = NAMESPACE + "float";

    /**
     * {@code decimal} XML Schema data type.
     */
    public static final String DECIMAL = NAMESPACE + "decimal";

    /**
     * {@code date} XML Schema data type.
     */
    public static final String DATE = NAMESPACE + "date";

    /**
     * {@code time} XML Schema data type.
     */
    public static final String TIME = NAMESPACE + "time";

    /**
     * {@code dateTime} XML Schema data type.
     */
    public static final String DATETIME = NAMESPACE + "dateTime";

    /**
     * {@code duration} XML Schema data type.
     */
    public static final String DURATION = NAMESPACE + "duration";

    /**
     * {@code string} XML Schema data type.
     */
    public static final String STRING = NAMESPACE + "string";

    /**
     * {@code normalizedString} XML Schema data type.
     */
    public static final String NORMALIZED_STRING = NAMESPACE + "normalizedString";

    /**
     * {@code anyURI} XML Schema data type.
     */
    public static final String ANY_URI = NAMESPACE + "anyURI";

    private XSD() {
        throw new AssertionError();
    }
}
