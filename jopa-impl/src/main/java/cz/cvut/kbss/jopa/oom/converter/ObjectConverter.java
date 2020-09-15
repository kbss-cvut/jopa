package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.NamedResource;

/**
 * Allows to convert values between arbitrary types.
 * <p>
 * The main intended use is for annotation property mapping to attributes of type {@link Object}, which can hold both
 * literal values and references to other individuals. In that case, the loaded instance is a {@link NamedResource} and
 * needs to be transformed to a {@link java.net.URI} to prevent the internal OntoDriver API from leaking into the
 * application.
 * <p>
 * Similarly, OntoDriver API's {@link LangString} is transformed to {@link MultilingualString}.
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
        if (value instanceof NamedResource) {
            return ((NamedResource) value).getIdentifier();
        } else if (value instanceof LangString) {
            final MultilingualString ms = new MultilingualString();
            final LangString ls = (LangString) value;
            ms.set(ls.getLanguage().orElse(null), ls.getValue());
            return ms;
        }
        return value;
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return true;
    }
}
