package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.CommonVocabulary;

import java.util.Objects;

class LongParameterValue extends ParameterValue {

    private final long value;

    public LongParameterValue(Long value) {
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public Long getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "\"" + value + "\"^^<" + CommonVocabulary.XSD_LONG + ">";
    }
}
