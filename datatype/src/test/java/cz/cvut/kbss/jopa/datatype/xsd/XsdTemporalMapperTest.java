/**
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.exception.DatatypeMappingException;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;

import java.time.*;
import java.time.chrono.ThaiBuddhistDate;
import java.time.format.DateTimeFormatter;
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
        assertEquals(value.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME), literal.getLexicalForm());
        assertEquals(XSD.DATETIME, literal.getDatatype());
    }

    @Test
    void mapTemporalAccessorReturnsXsdDateTimeWithOffsetForLocalDateTime() {
        final LocalDateTime value = LocalDateTime.now().truncatedTo(ChronoUnit.MILLIS);
        final Literal literal = XsdTemporalMapper.map(value);
        ZoneOffset offset = ZoneId.systemDefault().getRules().getOffset(value);
        OffsetDateTime offsetDateTime = OffsetDateTime.of(value.toLocalDate(), value.toLocalTime(), offset);
        assertEquals(offsetDateTime.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME), literal.getLexicalForm());
        assertEquals(XSD.DATETIME, literal.getDatatype());
    }

    @Test
    void mapTemporalAccessorReturnsXsdDateTimeWithoutOffsetForInstant() {
        final Instant value = Instant.now();
        final Literal literal = XsdTemporalMapper.map(value);
        assertEquals(value.atOffset(ZoneOffset.UTC).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                literal.getLexicalForm());
        assertEquals(XSD.DATETIME, literal.getDatatype());
    }

    @Test
    void mapTemporalAccessorReturnsXsdDateTimeWithOffsetForZonedDateTime() {
        final ZonedDateTime value = ZonedDateTime.now();
        final Literal literal = XsdTemporalMapper.map(value);
        assertEquals(value.toOffsetDateTime().format(DateTimeFormatter.ISO_OFFSET_DATE_TIME), literal.getLexicalForm());
        assertEquals(XSD.DATETIME, literal.getDatatype());
    }

    @Test
    void mapTemporalAccessorThrowsDatatypeMappingExceptionForUnsupportedTemporalRepresentation() {
        final ThaiBuddhistDate value = ThaiBuddhistDate.now();
        assertThrows(DatatypeMappingException.class, () -> XsdTemporalMapper.map(value));
    }

    @Test
    void mapTemporalAccessorReturnsXsdTimeForOffsetTime() {
        final OffsetTime value = OffsetTime.now().truncatedTo(ChronoUnit.MILLIS);
        final Literal literal = XsdTemporalMapper.map(value);
        assertEquals(value.format(DateTimeFormatter.ISO_OFFSET_TIME), literal.getLexicalForm());
        assertEquals(XSD.TIME, literal.getDatatype());
    }

    @Test
    void mapTemporalAccessorReturnsXsdTimeWithOffsetForLocalTime() {
        final LocalTime value = LocalTime.now().truncatedTo(ChronoUnit.MILLIS);
        final Literal literal = XsdTemporalMapper.map(value);
        ZoneOffset offset = ZoneId.systemDefault().getRules().getOffset(LocalDateTime.now());
        assertEquals(value.atOffset(offset).format(DateTimeFormatter.ISO_OFFSET_TIME), literal.getLexicalForm());
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
