package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.exception.UnsupportedTypeTransformationException;

import java.net.URI;

/**
 * Converter to {@link java.net.URI}.
 * <p>
 * Only {@code String} values are supported.
 */
public class ToURIConverter implements ConverterWrapper<URI, Object> {
    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return String.class.equals(type);
    }

    @Override
    public Object convertToAxiomValue(URI value) {
        return value.toString();
    }

    @Override
    public URI convertToAttribute(Object value) {
        try {
            return URI.create(value.toString());
        } catch (IllegalArgumentException e) {
            throw new UnsupportedTypeTransformationException("Unable to convert value " + value + " to URI.", e);
        }
    }
}
