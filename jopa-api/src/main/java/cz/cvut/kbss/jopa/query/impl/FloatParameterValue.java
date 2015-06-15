package cz.cvut.kbss.jopa.query.impl;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.query.ParameterValue;

import java.util.Objects;

/**
 * @author kidney
 */
public class FloatParameterValue extends ParameterValue {

    private final float value;

    public FloatParameterValue(Float value) {
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public Object getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "\"" + value + "\"^^<" + CommonVocabulary.XSD_FLOAT + ">";
    }
}
