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
