package cz.cvut.kbss.jopa.oom.converter;

public class ToDoubleConverter implements ConverterWrapper<Double, Object> {

    @Override
    public Object convertToAxiomValue(Double value) {
        return value;
    }

    @Override
    public Double convertToAttribute(Object value) {
        assert value != null;
        assert supportsAxiomValueType(value.getClass());
        return ((Number) value).doubleValue();
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return Number.class.isAssignableFrom(type);
    }
}
