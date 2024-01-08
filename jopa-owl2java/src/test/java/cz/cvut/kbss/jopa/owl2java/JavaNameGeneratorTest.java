package cz.cvut.kbss.jopa.owl2java;

import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.model.IRI;

import static org.junit.jupiter.api.Assertions.assertEquals;

class JavaNameGeneratorTest {

    private static final String PREFIX = "http://onto.fel.cvut.cz/ontologies/owl2java-test/";

    private final JavaNameGenerator sut = new JavaNameGenerator();

    @Test
    void toCamelCaseNotationCamelCasesStringsSeparatedByUnderscore() {
        assertEquals("TestValueCamelCased", JavaNameGenerator.toCamelCaseNotation("test_value_camel_cased"));
    }

    @Test
    void generateJavaNameForIriGeneratesValidJavaIdentifiersForIriWithNonAsciiCharacters() {
        final IRI iri = IRI.create(PREFIX + "navržený-pojem");
        assertEquals("navrzeny_pojem", sut.generateJavaNameForIri(iri));
    }

    @Test
    void generateJavaNameForIriSanitizesJavaKeywords() {
        final IRI iri = IRI.create(PREFIX + "volatile");
        assertEquals("_volatile", sut.generateJavaNameForIri(iri));
    }

    @Test
    void generateJavaNameUsesFragmentWhenIriContainsIt() {
        final IRI iri = IRI.create("http://onto.fel.cvut.cz/ontologies/owl2java-test#Fragment");
        assertEquals("Fragment", sut.generateJavaNameForIri(iri));
    }
}
