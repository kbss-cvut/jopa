package cz.cvut.kbss.jopa.oom.converter;

/**
 * Default converter implementing identity function - it just returns the provided argument.
 */
public final class DefaultConverterWrapper implements ConverterWrapper<Object, Object> {

    public static final DefaultConverterWrapper INSTANCE = new DefaultConverterWrapper();

    private DefaultConverterWrapper() {
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return false;
    }

    @Override
    public Object convertToAxiomValue(Object value) {
        return value;
    }

    @Override
    public Object convertToAttribute(Object value) {
        return value;
    }
}
