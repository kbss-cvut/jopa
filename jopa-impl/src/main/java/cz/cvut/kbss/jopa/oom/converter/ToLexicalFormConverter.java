package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.ontodriver.model.NamedResource;

/**
 * Converts literal lexical form to Java {@code String}.
 * <p>
 * This converter ensures seamless support for lexical forms.
 */
public class ToLexicalFormConverter implements ConverterWrapper<String, Object> {

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        // Anything but a NamedResource (i.e., a literal since anonymous individuals are not supported) can be transformed to lexical form
        return !NamedResource.class.isAssignableFrom(type);
    }

    @Override
    public Object convertToAxiomValue(String value) {
        // TODO
        return null;
    }

    @Override
    public String convertToAttribute(Object value) {
        assert value != null;
        return value.toString();
    }
}
