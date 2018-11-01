package cz.cvut.kbss.jopa.oom.converter;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ToFloatConverterTest {

    private ToFloatConverter converter = new ToFloatConverter();

    @Test
    public void toAttributeSupportsWideningIntegerConversion() {
        assertEquals(Float.valueOf(11), converter.convertToAttribute(Byte.valueOf((byte) 11)));
        assertEquals(Float.valueOf(117), converter.convertToAttribute(Short.valueOf((short) 117)));
        assertEquals(Float.valueOf(117), converter.convertToAttribute(117));
        assertEquals(Float.valueOf(117L), converter.convertToAttribute(117L));
    }

    @Test
    public void toAttributeSupportsIdentityConversion() {
        assertEquals(Float.valueOf(3.14F), converter.convertToAttribute(3.14F));
    }
}