package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.ontodriver.model.LangString;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ToStringConverterTest {

    private final ToStringConverter sut = new ToStringConverter();

    @Test
    void convertToAttributeReturnsStringValue() {
        final String value = "test";
        assertEquals(value, sut.convertToAttribute(value));
    }

    @Test
    void convertToAttributeHandlesLangStringValue() {
        final String str = "test";
        final LangString value = new LangString(str, "en");
        assertEquals(str, sut.convertToAttribute(value));
    }
}
