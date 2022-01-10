package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.time.Period;

import static cz.cvut.kbss.jopa.query.parameter.TemporalParameterValueTest.generateExpectedString;
import static org.junit.jupiter.api.Assertions.assertEquals;

class DurationParameterValueTest {

    @Test
    void getQueryStringReturnsXsdDurationForJavaDurationValue() {
        final Duration value = Duration.ofHours(Generators.randomPositiveInt(24));
        assertEquals(generateExpectedString(value.toString(), XSD.DURATION), new DurationParameterValue(value).getQueryString());
    }

    @Test
    void getQueryStringReturnsXsdDurationForJavaPeriodValue() {
        final Period value = Period.of(2, 8, Generators.randomPositiveInt(30));
        assertEquals(generateExpectedString(value.toString(), XSD.DURATION), new DurationParameterValue(value).getQueryString());
    }
}
