package cz.cvut.kbss.jopa.query.parameter;

/**
 * Represents a variable, i.e. an unset parameter value.
 * <p>
 * All query parameters start out as variable values, until real values are set for them.
 */
public class PositionalVariableParameterValue extends ParameterValue {

    private final Integer position;

    public PositionalVariableParameterValue(Integer position) {
        this.position = position;
    }

    @Override
    public Object getValue() {
        return null;
    }

    @Override
    public String getQueryString() {
        return "$" + position;
    }
}
