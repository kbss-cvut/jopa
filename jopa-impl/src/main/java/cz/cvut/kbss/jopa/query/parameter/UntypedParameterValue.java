package cz.cvut.kbss.jopa.query.parameter;

/**
 * Represents parameter value which is directly inserted into the query string, without any XSD type specification.
 */
class UntypedParameterValue implements ParameterValue {

    private final Object value;

    UntypedParameterValue(Object value) {
        this.value = value;
    }

    @Override
    public Object getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return value.toString();
    }
}
