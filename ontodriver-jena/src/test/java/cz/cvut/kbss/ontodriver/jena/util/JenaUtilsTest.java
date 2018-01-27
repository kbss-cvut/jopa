package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

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
}