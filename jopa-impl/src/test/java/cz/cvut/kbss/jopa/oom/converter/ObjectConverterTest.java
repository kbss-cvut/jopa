package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.URL;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

class ObjectConverterTest {

    private ObjectConverter sut = new ObjectConverter();

    @Test
    void convertToAxiomValueDoesNothingToLiteralValues() {
        final Object valueOne = "117";
        final Object valueTwo = 1237;
        assertSame(valueOne, sut.convertToAxiomValue(valueOne));
        assertSame(valueTwo, sut.convertToAxiomValue(valueTwo));
    }

    @Test
    void convertToAxiomValueTransformsIdentifierValueToNamedResource() throws Exception {
        final URL valueOne = Generators.createIndividualIdentifier().toURL();
        final URI valueTwo = Generators.createIndividualIdentifier();
        assertEquals(NamedResource.create(valueOne.toString()), sut.convertToAxiomValue(valueOne));
        assertEquals(NamedResource.create(valueTwo), sut.convertToAxiomValue(valueTwo));
    }

    @Test
    void convertToAttributeDoesNothingToLiteralValues() {
        final Object valueOne = new Date();
        final Object valueTwo = 1237;
        assertSame(valueOne, sut.convertToAttribute(valueOne));
        assertSame(valueTwo, sut.convertToAttribute(valueTwo));
    }

    @Test
    void convertToAttributeTransformsNamedResourceToUri() {
        final NamedResource value = NamedResource.create(Generators.createIndividualIdentifier());
        assertEquals(value.getIdentifier(), sut.convertToAttribute(value));
    }
}
