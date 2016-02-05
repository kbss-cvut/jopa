package cz.cvut.kbss.ontodriver.sesame;

import org.openrdf.model.*;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.model.vocabulary.XMLSchema;

import java.net.URL;
import java.util.Date;
import java.util.logging.Logger;

/**
 * Utility methods for the Sesame driver.
 *
 * @author ledvima1
 */
public final class SesameUtils {

    private SesameUtils() {
        // Private constructor
    }

    /**
     * Gets value of the specified data property literal as the corresponding Java object. Primitives are returned
     * boxed.
     *
     * @param literal DataProperty value
     * @return Java value corresponding to the XML Schema datatype of the literal
     * @throws IllegalArgumentException If literal's datatype is not supported
     */
    public static Object getDataPropertyValue(Literal literal) {
        assert literal != null;

        final URI datatype = literal.getDatatype();
        if (datatype == null || datatype.equals(XMLSchema.STRING)
                || datatype.equals(XMLSchema.NORMALIZEDSTRING) || datatype.equals(RDF.LANGSTRING)) {
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
     * Creates Sesame literal from the specified value, which can be used as data property object.
     *
     * @param value    The value to transform
     * @param language Language to add to string literals
     * @param vf       Sesame value factory
     * @return Sesame Literal
     * @throws IllegalArgumentException If the type of the value is not supported
     */
    public static Literal createDataPropertyLiteral(Object value, String language, ValueFactory vf) {
        assert value != null;

        if (value instanceof Integer) {
            return vf.createLiteral((Integer) value);
        } else if (value instanceof String) {
            return vf.createLiteral((String) value, language);
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
     * Resolves whether the specified value is a resource identifier. </p>
     *
     * @param value The value to check
     * @return {@code true} if the value is either an URI or an URL
     */
    public static boolean isResourceIdentifier(Object value) {
        return value instanceof java.net.URI || value instanceof URL || value instanceof URI;
    }

    /**
     * Constructs a Sesame URI from the specified java.net.URI.
     *
     * @param javaUri The uri to convert
     * @param factory ValueFactory used for the conversion
     * @return Sesame URI
     */
    public static URI toSesameUri(java.net.URI javaUri, ValueFactory factory) {
        return (javaUri != null ? factory.createURI(javaUri.toString()) : null);
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
            Logger.getLogger(SesameUtils.class.getName()).severe("Sesame resource is not a valid URI: " + e);
            return null;
        }
    }
}
