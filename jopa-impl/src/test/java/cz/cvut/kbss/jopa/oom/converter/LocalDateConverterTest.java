package cz.cvut.kbss.jopa.oom.converter;

import org.junit.Test;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

public class LocalDateConverterTest {

    private final LocalDateConverter sut = new LocalDateConverter();

    @Test
    public void convertToAxiomValueTransformsLocalDateToJavaUtilDate() {
        final LocalDate value = LocalDate.now();
        final Date result = sut.convertToAxiomValue(value);
        assertNotNull(result);
        assertEquals(java.sql.Date.valueOf(value), result);
    }

    @Test
    public void convertToAttributeTransformsJavaUtilDateToLocalDate() {
        final Date value = new Date();
        final LocalDate result = sut.convertToAttribute(value);
        assertNotNull(result);
        assertEquals(value.toInstant().atZone(ZoneId.systemDefault()).toLocalDate(), result);
    }

    @Test
    public void convertToAxiomReturnsNullForNullInput() {
        assertNull(sut.convertToAxiomValue(null));
    }

    @Test
    public void convertToAttributeReturnsNullForNullInput() {
        assertNull(sut.convertToAttribute(null));
    }
}