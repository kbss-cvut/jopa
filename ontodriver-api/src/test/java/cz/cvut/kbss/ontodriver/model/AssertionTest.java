package cz.cvut.kbss.ontodriver.model;

import org.junit.jupiter.api.Test;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.*;

class AssertionTest {

    private static final URI ID = URI.create("http://onto.fel.cvut.cz/ontologies/jopa/ontodriver/test");

    @Test
    void hasLanguageReturnsFalseWhenLanguageIsExplicitlySetToNull() {
        final Assertion sut = Assertion.createDataPropertyAssertion(ID, null, false);
        assertFalse(sut.hasLanguage());
    }

    @Test
    void equalsReturnsFalseWhenAssertionsDifferInLanguage() {
        final Assertion aOne = Assertion.createDataPropertyAssertion(ID, "cs", false);
        final Assertion aTwo = Assertion.createDataPropertyAssertion(ID, "en", false);
        assertNotEquals(aOne, aTwo);
    }

    @Test
    void equalsReturnsFalseWhenAssertionsDifferInInferenceType() {
        final Assertion aOne = Assertion.createDataPropertyAssertion(ID, false);
        final Assertion aTwo = Assertion.createDataPropertyAssertion(ID, true);
        assertNotEquals(aOne, aTwo);
    }
}