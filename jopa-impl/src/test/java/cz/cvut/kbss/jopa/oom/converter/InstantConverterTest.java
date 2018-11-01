package cz.cvut.kbss.jopa.oom.converter;

import org.junit.Test;

import java.time.Instant;
import java.util.Date;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class InstantConverterTest {

    private final InstantConverter sut = new InstantConverter();

    @Test
    public void convertToAxiomValueTransformsInstantToJavaUtilDate() {
        final Instant value = Instant.now();
        final Date result = sut.convertToAxiomValue(value);
        assertEquals(Date.from(value), result);
    }

    @Test
    public void convertToAttributeTransformsJavaUtilDateToInstant() {
        final Date value = new Date();
        final Instant result = sut.convertToAttribute(value);
        assertEquals(value.toInstant(), result);
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