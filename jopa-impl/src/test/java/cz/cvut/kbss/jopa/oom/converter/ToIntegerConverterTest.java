package cz.cvut.kbss.jopa.oom.converter;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ToIntegerConverterTest {

    private ToIntegerConverter converter = new ToIntegerConverter();

    @Test
    public void toAttributeSupportsWideningIntegerConversion() {
        assertEquals(Integer.valueOf(11), converter.convertToAttribute(Byte.valueOf((byte) 11)));
        assertEquals(Integer.valueOf(117), converter.convertToAttribute(Short.valueOf((short) 117)));
    }

    @Test
    public void toAttributeSupportsIdentityConversion() {
        assertEquals(Integer.valueOf(117), converter.convertToAttribute(117));
    }
}