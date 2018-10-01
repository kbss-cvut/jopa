package cz.cvut.kbss.jopa.oom.converter;

import org.junit.Test;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class ZonedDateTimeConverterTest {

    private final ZonedDateTimeConverter sut = new ZonedDateTimeConverter();

    @Test
    public void convertToAxiomValueTransformsZonedDateTimeToJavaUtilDate() {
        final ZonedDateTime value = ZonedDateTime.now();
        final Date result = sut.convertToAxiomValue(value);
        assertEquals(Date.from(value.toInstant()), result);
    }

    @Test
    public void convertToAttributeTransformsJavaUtilDateToZonedDateTime() {
        final Date value = new Date();
        final ZonedDateTime result = sut.convertToAttribute(value);
        assertEquals(value.toInstant().atZone(ZoneId.systemDefault()), result);
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