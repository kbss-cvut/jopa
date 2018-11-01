package cz.cvut.kbss.jopa.oom.converter;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ToShortConverterTest {

    private ToShortConverter converter = new ToShortConverter();

    @Test
    public void toAttributeSupportsWideningIntegerConversion() {
        assertEquals(Short.valueOf((short) 11), converter.convertToAttribute(Byte.valueOf((byte) 11)));
    }

    @Test
    public void toAttributeSupportsIdentityConversion() {
        assertEquals(Short.valueOf((short) 11), converter.convertToAttribute(Short.valueOf((short) 11)));
    }
}