package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.model.AttributeConverter;

import java.util.Objects;

/**
 * Instances of this class are used to wrap user-defined {@link AttributeConverter} implementation.
 */
public class CustomConverterWrapper<X, Y> implements ConverterWrapper<X, Y> {

    private final AttributeConverter<X, Y> wrapped;
    private final Class<Y> axiomValueType;

    public CustomConverterWrapper(AttributeConverter<X, Y> wrapped, Class<Y> axiomValueType) {
        this.wrapped = Objects.requireNonNull(wrapped);
        this.axiomValueType = axiomValueType;
    }

    @Override
    public Y convertToAxiomValue(X value) {
        return wrapped.convertToAxiomValue(value);
    }

    @Override
    public X convertToAttribute(Y value) {
        return wrapped.convertToAttribute(value);
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return axiomValueType.isAssignableFrom(type);
    }
}
