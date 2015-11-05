package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.CommonVocabulary;

import java.util.Date;
import java.util.Objects;

class DateParameterValue extends ParameterValue {

    private final Date value;

    public DateParameterValue(Date value) {
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public Date getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "\"" + value + "\"^^<" + CommonVocabulary.XSD_DATETIME + ">";
    }
}
