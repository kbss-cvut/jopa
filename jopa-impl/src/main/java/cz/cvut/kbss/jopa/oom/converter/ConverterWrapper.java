package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.model.AttributeConverter;

import java.util.Objects;

public class ConverterWrapper<X, Y> implements AttributeConverter<X, Y> {

    private final AttributeConverter<X, Y> wrapped;

    public ConverterWrapper(AttributeConverter<X, Y> wrapped) {
        this.wrapped = Objects.requireNonNull(wrapped);
    }

    @Override
    public Y convertToAxiomValue(X value) {
        return wrapped.convertToAxiomValue(value);
    }

    @Override
    public X convertToAttribute(Y value) {
        return wrapped.convertToAttribute(value);
    }

    /**
     * Checks whether the wrapped converter supports converting the specified axiom value type.
     *
     * @param type Axiom value type
     * @return Whether the type is supported by this wrapper
     */
    public boolean supportsType(Class<?> type) {
        return false;
    }
}
