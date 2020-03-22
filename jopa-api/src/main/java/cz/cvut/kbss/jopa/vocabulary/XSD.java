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
public class XSD {

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
     * {@code short} XML Schema data type.
     */
    public static final String SHORT = NAMESPACE + "short";

    /**
     * {@code int} XML Schema data type.
     */
    public static final String INT = NAMESPACE + "int";

    /**
     * {@code integer} XML Schema data type.
     */
    public static final String INTEGER = NAMESPACE + "integer";

    /**
     * {@code long} XML Schema data type.
     */
    public static final String LONG = NAMESPACE + "long";

    /**
     * {@code double} XML Schema data type.
     */
    public static final String DOUBLE = NAMESPACE + "double";

    /**
     * {@code float} XML Schema data type.
     */
    public static final String FLOAT = NAMESPACE + "float";

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
     * {@code string} XML Schema data type.
     */
    public static final String STRING = NAMESPACE + "string";

    private XSD() {
        throw new AssertionError();
    }
}
