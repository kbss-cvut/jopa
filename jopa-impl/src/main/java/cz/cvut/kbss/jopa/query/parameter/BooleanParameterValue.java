package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.CommonVocabulary;

import java.util.Objects;

class BooleanParameterValue extends ParameterValue {

    private final boolean value;

    public BooleanParameterValue(Boolean value) {
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public Boolean getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "\"" + value + "\"^^<" + CommonVocabulary.XSD_BOOLEAN + ">";
    }
}
