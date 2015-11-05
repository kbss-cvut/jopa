package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.CommonVocabulary;

import java.util.Objects;

class FloatParameterValue extends ParameterValue {

    private final float value;

    public FloatParameterValue(Float value) {
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public Float getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "\"" + value + "\"^^<" + CommonVocabulary.XSD_FLOAT + ">";
    }
}
