package cz.cvut.kbss.jopa.oom.converter;

public class ToIntegerConverter implements ConverterWrapper<Integer, Object> {

    @Override
    public Object convertToAxiomValue(Integer value) {
        return value;
    }

    @Override
    public Integer convertToAttribute(Object value) {
        assert value != null;
        assert supportsAxiomValueType(value.getClass());
        return ((Number) value).intValue();
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return Integer.class.isAssignableFrom(type) || Short.class.isAssignableFrom(type) || Byte.class
                .isAssignableFrom(type);
    }
}
