package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.NamedResource;

/**
 * Allows to convert values between arbitrary types.
 * <p>
 * The main intended use is for annotation property mapping to attributes of type {@link Object}, which can hold both
 * literal values and references to other individuals. In that case, the loaded instance is a {@link NamedResource} and
 * needs to be transformed to a {@link java.net.URI} to prevent the internal OntoDriver API from leaking into the
 * application.
 * <p>
 * In all other cases, the values will be just returned without any conversion.
 */
public class ObjectConverter implements ConverterWrapper<Object, Object> {

    @Override
    public Object convertToAxiomValue(Object value) {
        if (IdentifierTransformer.isValidIdentifierType(value.getClass()) && !(value instanceof String)) {
            return NamedResource.create(IdentifierTransformer.valueAsUri(value));
        }
        return value;
    }

    @Override
    public Object convertToAttribute(Object value) {
        return value instanceof NamedResource ? ((NamedResource) value).getIdentifier() : value;
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return true;
    }
}
