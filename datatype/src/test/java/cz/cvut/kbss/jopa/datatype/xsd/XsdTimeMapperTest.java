package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.DatatypeMappingException;
import org.junit.jupiter.api.Test;

import java.time.*;
import java.time.format.DateTimeFormatter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class XsdTimeMapperTest {

    @Test
    void mapParsesTimeWithoutOffsetToOffsetTimeWithLocalOffset() {
        final int hour = 12;
        final int minute = 11;
        final int second = 10;
        final String literal = hour + ":" + minute + ":" + second;
        assertEquals(
                LocalTime.of(hour, minute, second).atOffset(ZoneId.systemDefault().getRules().getOffset(LocalDateTime.now())),
                XsdTimeMapper.map(literal));
    }

    @Test
    void mapParsesTimeWithZeroOffset() {
        final OffsetTime time = OffsetTime.now().withOffsetSameLocal(ZoneOffset.UTC);
        assertEquals(time, XsdTimeMapper.map(time.format(DateTimeFormatter.ISO_TIME)));
    }

    @Test
    void mapParsesTimeWithExplicitOffset() {
        final OffsetTime time = OffsetTime.now();
        assertEquals(time, XsdTimeMapper.map(time.format(DateTimeFormatter.ISO_TIME)));
    }

    @Test
    void mapThrowsDatatypeMappingExceptionWhenTimeCannotBeParsed() {
        final String invalidValue = "12:78:da";
        assertThrows(DatatypeMappingException.class, () -> XsdTimeMapper.map(invalidValue));
    }
}
