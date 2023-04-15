package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.exception.InvalidEnumMappingException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import static org.junit.jupiter.api.Assertions.*;

class OrdinalEnumConverterTest {

    private final OrdinalEnumConverter<OWLClassM.Severity> sut = new OrdinalEnumConverter<>(OWLClassM.Severity.class);

    @ParameterizedTest
    @EnumSource(OWLClassM.Severity.class)
    void convertToAxiomValueReturnsIntegerRepresentingOrdinalValueOfSpecifiedConstant(OWLClassM.Severity value) {
        assertEquals(value.ordinal(), sut.convertToAxiomValue(value));
    }

    @ParameterizedTest
    @EnumSource(OWLClassM.Severity.class)
    void convertToAttributeResolvesEnumConstantFromSpecifiedOrdinal(OWLClassM.Severity value) {
        assertEquals(value, sut.convertToAttribute(value.ordinal()));
    }

    @Test
    void convertToAttributeThrowsInvalidEnumMappingExceptionForOrdinalOutOfEnumConstantsBounds() {
        assertThrows(InvalidEnumMappingException.class,
                     () -> sut.convertToAttribute(OWLClassM.Severity.values().length));
    }

    @Test
    void supportsReturnsTrueForOrdinalDatatypesThatCanBeCastToInteger() {
        assertTrue(sut.supportsAxiomValueType(Integer.class));
        assertTrue(sut.supportsAxiomValueType(Short.class));
        assertTrue(sut.supportsAxiomValueType(Byte.class));
        assertFalse(sut.supportsAxiomValueType(Long.class));
        assertFalse(sut.supportsAxiomValueType(String.class));
    }
}