package cz.cvut.kbss.jopa.query.parameter;

import java.util.Objects;

class StringParameterValue extends ParameterValue {

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
    public String getValue() {
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
