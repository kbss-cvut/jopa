package cz.cvut.kbss.jopa.oom.converter;

public class ToShortConverter implements ConverterWrapper<Short, Object> {

    @Override
    public Object convertToAxiomValue(Short value) {
        return value;
    }

    @Override
    public Short convertToAttribute(Object value) {
        assert value != null;
        assert supportsAxiomValueType(value.getClass());
        return ((Number) value).shortValue();
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return Short.class.isAssignableFrom(type) || Byte.class.isAssignableFrom(type);
    }
}
