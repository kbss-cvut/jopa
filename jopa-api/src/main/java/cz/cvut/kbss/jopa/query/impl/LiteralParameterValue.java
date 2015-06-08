package cz.cvut.kbss.jopa.query.impl;

import cz.cvut.kbss.jopa.query.ParameterValue;

/**
 * Represents a literal value that will be passed into the query.
 *
 * @author kidney
 */
public class LiteralParameterValue extends ParameterValue {

    private final Object value;

    public LiteralParameterValue(Object value) {
        assert value != null;
        this.value = value;
    }

    @Override
    public String getValue() {
        return value.toString();
    }

    @Override
    public String toString() {
        return getValue();
    }
}
