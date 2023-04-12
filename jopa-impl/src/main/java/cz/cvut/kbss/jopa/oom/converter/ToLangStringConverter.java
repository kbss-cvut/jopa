package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.ontodriver.model.LangString;

/**
 * Converts language tagged and language-less values to {@link LangString} attributes.
 * <p>
 * This is for the rare case that {@link LangString} is used as attribute type instead of the recommended {@link
 * cz.cvut.kbss.jopa.model.MultilingualString}.
 */
public class ToLangStringConverter implements ConverterWrapper<LangString, Object> {
    @Override
    public Object convertToAxiomValue(LangString value) {
        return value;
    }

    @Override
    public LangString convertToAttribute(Object value) {
        assert value != null;
        assert supportsAxiomValueType(value.getClass());
        return value instanceof LangString ? (LangString) value : new LangString(value.toString());
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return String.class.equals(type) || LangString.class.isAssignableFrom(type);
    }
}
