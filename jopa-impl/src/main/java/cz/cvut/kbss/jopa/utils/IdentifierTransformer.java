package cz.cvut.kbss.jopa.utils;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;

/**
 * @author ledvima1
 */
class IdentifierTransformer {

    /**
     * Transforms the specified value to the target type (if possible).
     *
     * @param value      The value to transform
     * @param targetType Target type
     * @return The transformed value
     */
    public Object transformToType(Object value, Class<?> targetType) {
        if (targetType.isAssignableFrom(value.getClass())) {
            return value;
        }
        if (URI.class.isAssignableFrom(targetType)) {
            return URI.create(value.toString());
        } else if (URL.class.isAssignableFrom(targetType)) {
            return valueAsUrl(value);
        } else if (String.class.isAssignableFrom(targetType)) {
            return value.toString();
        }
        throw new IllegalArgumentException(
                "The specified value " + value + " cannot be transformed to target type " + targetType);
    }

    URI valueAsUri(Object value) {
        if (value instanceof URI) {
            return (URI) value;
        } else {
            return URI.create(value.toString());
        }
    }

    private URL valueAsUrl(Object value) {
        try {
            return new URL(value.toString());
        } catch (MalformedURLException e) {
            throw new IllegalArgumentException("The identifier is not a valid URL.", e);
        }
    }
}
