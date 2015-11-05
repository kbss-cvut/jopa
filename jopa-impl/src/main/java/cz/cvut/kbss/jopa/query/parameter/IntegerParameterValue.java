package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.CommonVocabulary;

import java.util.Objects;

class IntegerParameterValue extends ParameterValue {

    private final int value;

    public IntegerParameterValue(Integer value) {
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public Integer getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "\"" + value + "\"^^<" + CommonVocabulary.XSD_INTEGER + ">";
    }
}
