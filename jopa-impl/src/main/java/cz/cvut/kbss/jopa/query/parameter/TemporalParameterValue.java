package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.datatype.xsd.XsdTemporalMapper;

import java.time.temporal.TemporalAccessor;
import java.util.Objects;

/**
 * Query parameter value representation for date/time.
 * <p>
 * Based on the Java 8 {@link TemporalAccessor} implementations. Query string mapping uses XSD datatypes.
 */
public class TemporalParameterValue extends AbstractParameterValue {

    private final TemporalAccessor value;

    public TemporalParameterValue(TemporalAccessor value) {
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
