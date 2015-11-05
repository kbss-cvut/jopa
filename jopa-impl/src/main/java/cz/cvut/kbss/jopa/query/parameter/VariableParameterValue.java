package cz.cvut.kbss.jopa.query.parameter;

/**
 * Represents a variable, i.e. an unset parameter value.
 * <p>
 * All query parameters start out as variable values, until real values are set for them.
 */
class VariableParameterValue extends ParameterValue {

    private final String variableName;

    VariableParameterValue(String variableName) {
        this.variableName = variableName;
    }

    @Override
    public String getValue() {
        return null;
    }

    @Override
    public String getQueryString() {
        return "?" + variableName;
    }
}
