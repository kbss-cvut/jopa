package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.exception.UnsupportedTypeTransformationException;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * Converter to {@link java.net.URL}.
 * <p>
 * Only {@code String} values are supported.
 */
public class ToURLConverter implements ConverterWrapper<URL, Object> {

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return String.class.equals(type);
    }

    @Override
    public Object convertToAxiomValue(URL value) {
        return value.toString();
    }

    @Override
    public URL convertToAttribute(Object value) {
        try {
            return new URL(value.toString());
        } catch (MalformedURLException e) {
            throw new UnsupportedTypeTransformationException("Unable to convert value " + value + " to URL.", e);
        }
    }
}
