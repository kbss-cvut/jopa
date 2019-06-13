package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ToLexicalFormConverterTest {

    private final ToLexicalFormConverter sut = new ToLexicalFormConverter();

    @Test
    void convertToAttributeReturnsStringValueOfSpecifiedObject() {
        final Integer value = 117;
        assertTrue(sut.supportsAxiomValueType(value.getClass()));
        final String result = sut.convertToAttribute(value);
        assertEquals(value.toString(), result);
    }

    @Test
    void supportsAxiomValueTypeReturnsFalseForNamedResource() {
        assertFalse(sut.supportsAxiomValueType(NamedResource.class));
    }
}