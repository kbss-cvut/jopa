package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.CommonVocabulary;

import java.util.Objects;

class DoubleParameterValue extends ParameterValue {

    private final double value;

    public DoubleParameterValue(Double value) {
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public Double getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "\"" + value + "\"^^<" + CommonVocabulary.XSD_DOUBLE + ">";
    }
}
