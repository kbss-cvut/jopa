package cz.cvut.kbss.jopa.oom.converter;

/**
 * Converts literal lexical form to Java {@code String}.
 * <p>
 * This converter ensures seamless support for lexical forms.
 */
public class ToLexicalFormConverter implements ConverterWrapper<String, Object> {

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return true;
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
