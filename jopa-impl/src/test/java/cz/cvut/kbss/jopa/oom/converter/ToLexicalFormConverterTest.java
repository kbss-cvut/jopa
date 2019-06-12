package cz.cvut.kbss.jopa.oom.converter;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ToLexicalFormConverterTest {

    @Test
    void convertToAttributeReturnsStringValueOfSpecifiedObject() {
        final ToLexicalFormConverter sut = new ToLexicalFormConverter();
        final Integer value = 117;
        final String result = sut.convertToAttribute(value);
        assertEquals(value.toString(), result);
    }
}