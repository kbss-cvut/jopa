package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.vocabulary.XSD;

import java.time.LocalTime;
import java.time.OffsetTime;

/**
 * Parameter values which represent XSD time.
 * <p>
 * Currently, these are {@link LocalTime} and {@link OffsetTime}.
 */
class TimeParameterValue extends AbstractParameterValue {

    private final Object value;

    TimeParameterValue(Object value) {
        assert value instanceof LocalTime || value instanceof OffsetTime;
        this.value = value;
    }

    @Override
    public Object getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "\"" + value + "\"^^<" + XSD.TIME + ">";
    }
}
