package cz.cvut.kbss.jopa.oom.converter;

import org.junit.Test;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

public class LocalDateTimeConverterTest {

    private final LocalDateTimeConverter sut = new LocalDateTimeConverter();

    @Test
    public void convertToAxiomValueTransformsLocalDateTimeToJavaUtilDate() {
        final LocalDateTime value = LocalDateTime.now();
        final Date result = sut.convertToAxiomValue(value);
        assertNotNull(result);
        assertEquals(java.sql.Timestamp.valueOf(value), result);
    }

    @Test
    public void convertToAttributeTransformsJavaUtilDateToLocalDateTime() {
        final Date value = new Date();
        final LocalDateTime result = sut.convertToAttribute(value);
        assertEquals(value.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime(), result);
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