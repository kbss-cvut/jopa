package cz.cvut.kbss.jopa.oom.converter;

public class ToLongConverter implements ConverterWrapper<Long, Object> {

    @Override
    public Object convertToAxiomValue(Long value) {
        return value;
    }

    @Override
    public Long convertToAttribute(Object value) {
        assert value != null;
        assert supportsAxiomValueType(value.getClass());
        return ((Number) value).longValue();
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return Long.class.isAssignableFrom(type) || Integer.class.isAssignableFrom(type) || Short.class
                .isAssignableFrom(type) || Byte.class.isAssignableFrom(type);
    }
}
