package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.exception.DatatypeMappingException;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;

import java.time.*;
import java.time.chrono.ThaiBuddhistDate;
import java.time.temporal.ChronoUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class XsdTemporalMapperTest {

    @Test
    void mapTemporalAmountReturnsXsdDurationLiteralForJavaDuration() {
        final Duration value = Duration.of(100, ChronoUnit.SECONDS);
        final Literal result = XsdTemporalMapper.map(value);
        assertEquals(XSD.DURATION, result.getDatatype());
        assertEquals(value.toString(), result.getLexicalForm());
    }

    @Test
    void mapTemporalAmountReturnsXsdDurationLiteralForJavaPeriod() {
        final LocalDate today = LocalDate.now();
        final Period value = Period.of(today.getYear(), today.getMonthValue(), today.getDayOfMonth());
        final Literal result = XsdTemporalMapper.map(value);
        assertEquals(XSD.DURATION, result.getDatatype());
        assertEquals(value.toString(), result.getLexicalForm());
    }

    @Test
    void mapTemporalAccessorReturnsXsdDateTimeWithOffsetForOffsetDateTime() {
        final OffsetDateTime value = OffsetDateTime.now();
        final Literal literal = XsdTemporalMapper.map(value);
        assertEquals(value.toString(), literal.getLexicalForm());
        assertEquals(XSD.DATETIME, literal.getDatatype());
    }

    @Test
    void mapTemporalAccessorReturnsXsdDateTimeWithOffsetForLocalDateTime() {
        final LocalDateTime value = LocalDateTime.now();
        final Literal literal = XsdTemporalMapper.map(value);
        ZoneOffset offset = ZoneId.systemDefault().getRules().getOffset(value);
        OffsetDateTime offsetDateTime = OffsetDateTime.of(value.toLocalDate(), value.toLocalTime(), offset);
        assertEquals(offsetDateTime.toString(), literal.getLexicalForm());
        assertEquals(XSD.DATETIME, literal.getDatatype());
    }

    @Test
    void mapTemporalAccessorReturnsXsdDateTimeWithoutOffsetForInstant() {
        final Instant value = Instant.now();
        final Literal literal = XsdTemporalMapper.map(value);
        assertEquals(value.atOffset(ZoneOffset.UTC).toString(), literal.getLexicalForm());
        assertEquals(XSD.DATETIME, literal.getDatatype());
    }

    @Test
    void mapTemporalAccessorReturnsXsdDateTimeWithOffsetForZonedDateTime() {
        final ZonedDateTime value = ZonedDateTime.now();
        final Literal literal = XsdTemporalMapper.map(value);
        assertEquals(value.toOffsetDateTime().toString(), literal.getLexicalForm());
        assertEquals(XSD.DATETIME, literal.getDatatype());
    }

    @Test
    void mapTemporalAccessorThrowsDatatypeMappingExceptionForUnsupportedTemporalRepresentation() {
        final ThaiBuddhistDate value = ThaiBuddhistDate.now();
        assertThrows(DatatypeMappingException.class, () -> XsdTemporalMapper.map(value));
    }

    @Test
    void mapTemporalAccessorReturnsXsdTimeForOffsetTime() {
        final OffsetTime value = OffsetTime.now();
        final Literal literal = XsdTemporalMapper.map(value);
        assertEquals(value.toString(), literal.getLexicalForm());
        assertEquals(XSD.TIME, literal.getDatatype());
    }

    @Test
    void mapTemporalAccessorReturnsXsdTimeWithOffsetForLocalTime() {
        final LocalTime value = LocalTime.now();
        final Literal literal = XsdTemporalMapper.map(value);
        ZoneOffset offset = ZoneId.systemDefault().getRules().getOffset(LocalDateTime.now());
        assertEquals(value.atOffset(offset).toString(), literal.getLexicalForm());
        assertEquals(XSD.TIME, literal.getDatatype());
    }

    @Test
    void mapTemporalAccessorReturnsXsdDateForLocalDate() {
        final LocalDate value = LocalDate.now();
        final Literal literal = XsdTemporalMapper.map(value);
        assertEquals(value.toString(), literal.getLexicalForm());
        assertEquals(XSD.DATE, literal.getDatatype());
    }
}
