package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.MultilingualString;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.net.URI;
import java.util.Map;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ToMultilingualStringConverterTest {

    private final ToMultilingualStringConverter sut = new ToMultilingualStringConverter();

    @ParameterizedTest
    @MethodSource("supportedTypes")
    void supportsReturnsTrueForSupportedAxiomValueTypes(Class<?> type) {
        assertTrue(sut.supportsAxiomValueType(type));
    }

    static Stream<Arguments> supportedTypes() {
        return Stream.of(
                Arguments.of(MultilingualString.class),
                Arguments.of(LangString.class),
                Arguments.of(String.class)
        );
    }

    @Test
    void supportsReturnsFalseForUnsupportedAxiomValueType() {
        assertFalse(sut.supportsAxiomValueType(URI.class));
    }

    @Test
    void convertToAxiomValueReturnsOntoDriverMultilingualStringWithProvidedValue() {
        final cz.cvut.kbss.jopa.model.MultilingualString value = cz.cvut.kbss.jopa.model.MultilingualString.create("en", "Test");

        final Object result = sut.convertToAxiomValue(value);
        assertInstanceOf(MultilingualString.class, result);
        assertEquals(value.getValue(), ((MultilingualString) result).getValue());
    }

    @Test
    void convertToAttributeConvertsOntoDriverMultilingualStringToJopaMultilingualString() {
        final MultilingualString value = new MultilingualString(Map.of("en", "Test"));

        final cz.cvut.kbss.jopa.model.MultilingualString result = sut.convertToAttribute(value);
        assertEquals(value.getValue(), result.getValue());
    }

    @Test
    void convertToAttributeConvertsLangStringToJopaMultilingualString() {
        final LangString value = new LangString("Test", "cs");

        final cz.cvut.kbss.jopa.model.MultilingualString result = sut.convertToAttribute(value);
        assertEquals(cz.cvut.kbss.jopa.model.MultilingualString.create(value.getValue(), value.getLanguage()
                                                                                              .get()), result);
    }

    @Test
    void convertToAttributeConvertsPlainStringToJopaMultilingualString() {
        final cz.cvut.kbss.jopa.model.MultilingualString result = sut.convertToAttribute("Test");
        assertEquals(cz.cvut.kbss.jopa.model.MultilingualString.create("Test", null), result);
    }
}
