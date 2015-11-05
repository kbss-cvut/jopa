package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.CommonVocabulary;

import java.util.Objects;

class ShortParameterValue extends ParameterValue {

    private final short value;

    public ShortParameterValue(Short value) {
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public Short getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "\"" + value +"\"^^<" + CommonVocabulary.XSD_SHORT + ">";
    }
}
