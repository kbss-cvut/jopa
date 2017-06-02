package cz.cvut.kbss.ontodriver.owlapi.util;

import org.junit.Test;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLLiteral;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import static org.junit.Assert.*;

public class OwlapiUtilsTest {

    private static final String LANG = "en";

    private OWLDataFactory dataFactory = new OWLDataFactoryImpl();

    @Test
    public void doesLanguageMatchReturnsTrueWhenLanguageTagMatches() {
        final OWLLiteral literal = dataFactory.getOWLLiteral("test", LANG);
        assertTrue(OwlapiUtils.doesLanguageMatch(literal, LANG));
    }

    @Test
    public void doesLanguageMatchReturnsFalseWhenLanguageTagDoesNotMatch() {
        final OWLLiteral literal = dataFactory.getOWLLiteral("test", LANG);
        assertFalse(OwlapiUtils.doesLanguageMatch(literal, "cs"));
    }

    @Test
    public void doesLanguageMatchReturnsTrueWhenLanguageIsNotSpecified() {
        final OWLLiteral literal = dataFactory.getOWLLiteral("test", LANG);
        assertTrue(OwlapiUtils.doesLanguageMatch(literal, null));
    }

    @Test
    public void doesLanguageMatchReturnsTrueWhenLiteralHasNoLanguageTag() {
        final OWLLiteral literal = dataFactory.getOWLLiteral("test");
        assertTrue(OwlapiUtils.doesLanguageMatch(literal, LANG));
    }
}