package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.datatype.xsd.XsdTemporalMapper;

import java.time.temporal.TemporalAmount;
import java.util.Objects;

/**
 * Duration query parameter value representation.
 * <p>
 * Works for both {@link java.time.Duration} and {@link java.time.Period}, which are mapped to XSD duration.
 */
public class DurationParameterValue extends AbstractParameterValue {

    private final TemporalAmount value;

    public DurationParameterValue(TemporalAmount value) {
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public Object getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return XsdTemporalMapper.map(value).toString();
    }
}
