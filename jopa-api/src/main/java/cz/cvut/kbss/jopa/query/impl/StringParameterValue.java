package cz.cvut.kbss.jopa.query.impl;

import cz.cvut.kbss.jopa.query.ParameterValue;

import java.util.Objects;

/**
 * Created by kidney on 15.6.15.
 */
public class StringParameterValue extends ParameterValue {

    private final String value;
    private final String language;

    public StringParameterValue(String value) {
        this.value = Objects.requireNonNull(value);
        this.language = null;
    }

    public StringParameterValue(String value, String language) {
        this.value = Objects.requireNonNull(value);
        this.language = language;
    }

    @Override
    public Object getValue() {
        return value;
    }

    public String getLanguage() {
        return language;
    }

    @Override
    public String getQueryString() {
        return "\"" + value + "\"" + (language != null ? ("@" + language) : "");
    }
}
