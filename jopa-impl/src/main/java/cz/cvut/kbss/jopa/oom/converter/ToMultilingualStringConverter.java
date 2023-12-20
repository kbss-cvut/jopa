package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.ontodriver.model.LangString;

/**
 * Supports mapping selected value types to multilingual strings.
 * <p>
 * This is not a general-purpose converter, it should be used only by the mapping of referenced lists containing
 * multilingual string values.
 */
public class ToMultilingualStringConverter implements ConverterWrapper<MultilingualString, Object> {

    @Override
    public Object convertToAxiomValue(MultilingualString value) {
        return new cz.cvut.kbss.ontodriver.model.MultilingualString(value.getValue());
    }

    @Override
    public MultilingualString convertToAttribute(Object value) {
        final Class<?> type = value.getClass();
        assert supportsAxiomValueType(type);
        if (value instanceof cz.cvut.kbss.ontodriver.model.MultilingualString) {
            return new MultilingualString(((cz.cvut.kbss.ontodriver.model.MultilingualString) value).getValue());
        } else if (value instanceof LangString) {
            final LangString ls = (LangString) value;
            return MultilingualString.create(ls.getValue(), ls.getLanguage().orElse(null));
        } else {
            return MultilingualString.create(value.toString(), null);
        }
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return cz.cvut.kbss.ontodriver.model.MultilingualString.class.isAssignableFrom(type)
                || LangString.class.isAssignableFrom(type)
                || String.class.isAssignableFrom(type);
    }
}
