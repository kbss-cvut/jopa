package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.junit.Test;

import static org.junit.Assert.*;

public class JenaUtilsTest {

    @Test
    public void isResourceReturnsTrueForUri() {
        assertTrue(JenaUtils.isResourceIdentifier(Generator.generateUri()));
    }

    @Test
    public void isResourceReturnsTrueForNamedResource() {
        assertTrue(JenaUtils.isResourceIdentifier(NamedResource.create(Generator.generateUri())));
    }

    @Test
    public void isResourceReturnsTrueForStringUri() {
        assertTrue(JenaUtils.isResourceIdentifier(Generator.generateUri().toString()));
    }

    @Test
    public void isResourceReturnsFalseForNonStringValue() {
        assertFalse(JenaUtils.isResourceIdentifier(117));
    }

    @Test
    public void isResourceReturnsFalseForNonUriStringValue() {
        assertFalse(JenaUtils.isResourceIdentifier("jdf123"));
    }

    @Test
    public void literalToValueReturnsTypeRepresentedByLiteral() {
        assertEquals(117, JenaUtils.literalToValue(ResourceFactory.createTypedLiteral(117)));
        assertEquals(true, JenaUtils.literalToValue(ResourceFactory.createTypedLiteral(true)));
        assertEquals("test", JenaUtils.literalToValue(ResourceFactory.createTypedLiteral("test")));
    }

    @Test
    public void literalToValueTranslatesLongLiteralToJavaLong() {
        assertEquals(117L, JenaUtils.literalToValue(ResourceFactory.createTypedLiteral(117L)));
    }
}