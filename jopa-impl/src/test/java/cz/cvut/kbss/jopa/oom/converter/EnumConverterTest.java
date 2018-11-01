package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.exception.UnsupportedTypeTransformation;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class EnumConverterTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void convertToAxiomValueTransformsEnumValueToString() {
        final EnumConverter<OWLClassM.Severity> sut = new EnumConverter<>(OWLClassM.Severity.class);
        final Object result = sut.convertToAxiomValue(OWLClassM.Severity.HIGH);
        assertTrue(result instanceof String);
        assertEquals(OWLClassM.Severity.HIGH.toString(), result);
    }

    @Test
    public void convertToAttributeValueUsesEnumValueOf() {
        final EnumConverter<OWLClassM.Severity> sut = new EnumConverter<>(OWLClassM.Severity.class);
        final OWLClassM.Severity result = sut.convertToAttribute(OWLClassM.Severity.HIGH.toString());
        assertEquals(OWLClassM.Severity.HIGH, result);
    }

    @Test
    public void convertToAttributeValueThrowsUnsupportedTypeTransformationForUnknownEnumValue() {
        thrown.expect(UnsupportedTypeTransformation.class);
        final EnumConverter<OWLClassM.Severity> sut = new EnumConverter<>(OWLClassM.Severity.class);
        sut.convertToAttribute("test");
    }
}