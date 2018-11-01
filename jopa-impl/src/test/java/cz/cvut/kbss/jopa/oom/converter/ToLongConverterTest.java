package cz.cvut.kbss.jopa.oom.converter;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ToLongConverterTest {

    private ToLongConverter converter = new ToLongConverter();

    @Test
    public void toAttributeSupportsWideningIntegerConversion() {
        assertEquals(Long.valueOf(11), converter.convertToAttribute(Byte.valueOf((byte) 11)));
        assertEquals(Long.valueOf(117), converter.convertToAttribute(Short.valueOf((short) 117)));
        assertEquals(Long.valueOf(117), converter.convertToAttribute(117));
    }

    @Test
    public void toAttributeSupportsIdentityConversion() {
        assertEquals(Long.valueOf(117), converter.convertToAttribute(117L));
    }
}