package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.DatatypeMappingException;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import static org.junit.jupiter.api.Assertions.*;

class XsdDateMapperTest {

    @Test
    void mapParsesSpecifiedStringAndTransformsItToLocalDate() {
        final LocalDate date = LocalDate.now();
        assertEquals(date, XsdDateMapper.map(date.format(DateTimeFormatter.ISO_DATE)));
    }

    @Test
    void mapParsesSpecifiedZonedStringAndTransformsItToLocalDate() {
        final int year = 2000;
        final int month = 12;
        final int day = 20;
        final String value = year + "-" + month + "-" + day + "+02:00";
        assertEquals(LocalDate.of(year, month, day), XsdDateMapper.map(value));
    }

    @Test
    void mapThrowsDatatypeMappingExceptionWhenDateCannotBeParsed() {
        final String invalidValue = "123-321-41";
        assertThrows(DatatypeMappingException.class, () -> XsdDateMapper.map(invalidValue));
    }
}
