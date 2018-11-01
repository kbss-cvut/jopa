package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.model.AttributeConverter;

public interface ConverterWrapper<X, Y> extends AttributeConverter<X, Y> {

    /**
     * Checks whether the wrapped converter supports converting the specified axiom value type.
     *
     * @param type Axiom value type
     * @return Whether the type is supported by this wrapper
     */
    boolean supportsAxiomValueType(Class<?> type);
}
