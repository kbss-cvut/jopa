package cz.cvut.kbss.jopa.query.impl;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.query.ParameterValue;

import java.util.Objects;

/**
 * Created by kidney on 15.6.15.
 */
public class BooleanParameterValue extends ParameterValue {

    private final boolean value;

    public BooleanParameterValue(Boolean value) {
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public Object getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "\"" + value + "\"^^<" + CommonVocabulary.XSD_BOOLEAN + ">";
    }
}
