package cz.cvut.kbss.jopa.oom.converter;

public class ToStringConverter implements ConverterWrapper<String, Object> {

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return true;
    }

    @Override
    public Object convertToAxiomValue(String value) {
        return value;
    }

    @Override
    public String convertToAttribute(Object value) {
        return value.toString();
    }
}
