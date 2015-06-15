package cz.cvut.kbss.jopa.query.impl;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.query.ParameterValue;

import java.util.Objects;

/**
 * @author kidney
 */
public class DoubleParameterValue extends ParameterValue {

    private final double value;

    public DoubleParameterValue(Double value) {
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public Object getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "\"" + value + "\"^^<" + CommonVocabulary.XSD_DOUBLE + ">";
    }
}
