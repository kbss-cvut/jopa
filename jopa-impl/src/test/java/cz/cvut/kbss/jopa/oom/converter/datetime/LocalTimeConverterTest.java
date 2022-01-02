package cz.cvut.kbss.jopa.oom.converter.datetime;

import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;

import java.time.LocalTime;
import java.time.OffsetTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

class LocalTimeConverterTest {

    private final LocalTimeConverter sut = new LocalTimeConverter();

    @Test
    void convertToAttributeTransformsJavaOffsetDateTimeToLocalDateTime() {
        final OffsetTime value = OffsetTime.now();
        final LocalTime result = sut.convertToAttribute(value);
        assertEquals(value.toLocalTime(), result);
    }

    @Test
    void convertToAttributeTransformsLiteralToLocalTime() {
        final OffsetTime value = OffsetTime.now();
        final Literal literal = Literal.from(value.format(DateTimeFormatter.ISO_OFFSET_TIME), XSD.TIME);
        final LocalTime result = sut.convertToAttribute(literal);
        assertEquals(value.toLocalTime(), result);
    }

    @Test
    void supportsAxiomValueTypeReturnsTrueForOffsetDateTime() {
        assertTrue(sut.supportsAxiomValueType(OffsetTime.class));
        assertFalse(sut.supportsAxiomValueType(Date.class));
    }

    @Test
    void supportsAxiomValueTypeReturnsTrueForLiteral() {
        assertTrue(sut.supportsAxiomValueType(Literal.class));
    }

}