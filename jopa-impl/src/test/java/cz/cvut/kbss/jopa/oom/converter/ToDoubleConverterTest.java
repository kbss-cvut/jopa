package cz.cvut.kbss.jopa.oom.converter;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ToDoubleConverterTest {

    private ToDoubleConverter converter = new ToDoubleConverter();

    @Test
    public void toAttributeSupportsWideningIntegerConversion() {
        assertEquals(Double.valueOf(11), converter.convertToAttribute(Byte.valueOf((byte) 11)));
        assertEquals(Double.valueOf(117), converter.convertToAttribute(Short.valueOf((short) 117)));
        assertEquals(Double.valueOf(117), converter.convertToAttribute(117));
        assertEquals(Double.valueOf(117L), converter.convertToAttribute(117L));
        assertEquals(Double.valueOf(3.14), converter.convertToAttribute(3.14F), 0.001);
    }

    @Test
    public void toAttributeSupportsIdentityConversion() {
        assertEquals(Double.valueOf(Math.E), converter.convertToAttribute(Math.E));
    }
}