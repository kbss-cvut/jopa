package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.ontodriver.model.LangString;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ToLangStringConverterTest {

    private final ToLangStringConverter sut = new ToLangStringConverter();

    @Test
    void convertToAttributeReturnsValueWhenItIsLangStringAlready() {
        final LangString value = new LangString("test value " + Generators.randomInt(), "en");
        assertSame(value, sut.convertToAttribute(value));
    }

    @Test
    void convertToAttributeCreatesLangStringWithoutLanguageWhenStringIsProvided() {
        final String value = "test value" + Generators.randomInt();
        assertEquals(new LangString(value), sut.convertToAttribute(value));
    }
}