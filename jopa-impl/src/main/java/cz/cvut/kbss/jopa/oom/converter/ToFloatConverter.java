package cz.cvut.kbss.jopa.oom.converter;

public class ToFloatConverter implements ConverterWrapper<Float, Object> {

    @Override
    public Object convertToAxiomValue(Float value) {
        return value;
    }

    @Override
    public Float convertToAttribute(Object value) {
        assert value != null;
        assert supportsAxiomValueType(value.getClass());
        return ((Number) value).floatValue();
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return Float.class.isAssignableFrom(type) || Long.class.isAssignableFrom(type) || Integer.class
                .isAssignableFrom(type) || Short.class.isAssignableFrom(type) || Byte.class.isAssignableFrom(type);
    }
}
