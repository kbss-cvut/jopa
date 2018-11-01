package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.model.AttributeConverter;

/**
 * Default converter implementation returns the values as they are passed in.
 */
public class DefaultConverter implements AttributeConverter<Object, Object> {

    /**
     * Reusable instance of this converter.
     */
    public static final DefaultConverter INSTANCE = new DefaultConverter();

    @Override
    public Object convertToAxiomValue(Object value) {
        return value;
    }

    @Override
    public Object convertToAttribute(Object value) {
        return value;
    }
}
