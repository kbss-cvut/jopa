package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.vocabulary.XSD;

import java.time.LocalDate;
import java.util.Objects;

/**
 * Parameter value which represents an XSD date.
 */
class DateParameterValue extends AbstractParameterValue {

    private final LocalDate value;

    DateParameterValue(LocalDate value) {
        this.value = Objects.requireNonNull(value);
    }

    @Override
    public LocalDate getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "\"" + value + "\"^^<" + XSD.DATE + ">";
    }
}
