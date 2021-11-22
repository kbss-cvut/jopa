package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Literal;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.time.LocalTime;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.jupiter.api.Assertions.*;

class ToRdfLiteralConverterTest {

    @Test
    void convertToAxiomValueReturnsOntoDriverLiteralWithProvidedStringAsLexicalFormAndDatatype() {
        final ToRdfLiteralConverter sut = new ToRdfLiteralConverter(XSD.BOOLEAN);
        final Object result = sut.convertToAxiomValue(Boolean.TRUE.toString());
        assertThat(result, instanceOf(Literal.class));
        final Literal literalResult = (Literal) result;
        assertEquals(Boolean.TRUE.toString(), literalResult.getLexicalForm());
        assertEquals(XSD.BOOLEAN, literalResult.getDatatype());
    }

    @Test
    void convertToAttributeExtractsLexicalFormFromOntoDriverLiteral() {
        final ToRdfLiteralConverter sut = new ToRdfLiteralConverter(XSD.BOOLEAN);
        final Literal literal = new Literal(LocalTime.now().toString(), XSD.TIME);
        final String result = sut.convertToAttribute(literal);
        assertEquals(literal.getLexicalForm(), result);
    }

    @Test
    void supportsAxiomValueTypeReturnsFalseForNamedResource() {
        final ToRdfLiteralConverter sut = new ToRdfLiteralConverter(XSD.BOOLEAN);
        assertFalse(sut.supportsAxiomValueType(NamedResource.class));
    }

    @Test
    void supportsAxiomValueTypeReturnsFalseForUri() {
        final ToRdfLiteralConverter sut = new ToRdfLiteralConverter(XSD.BOOLEAN);
        assertFalse(sut.supportsAxiomValueType(URI.class));
    }

    @Test
    void convertToAttributeReturnsStringRepresentationOfProvidedValue() {
        final ToRdfLiteralConverter sut = new ToRdfLiteralConverter(XSD.BOOLEAN);
        assertEquals(Boolean.TRUE.toString(), sut.convertToAttribute(true));
    }

    @Test
    void convertToAttributeReturnsValueOfLangString() {
        final ToRdfLiteralConverter sut = new ToRdfLiteralConverter(RDF.LANG_STRING);
        final String value = "car";
        assertEquals(value, sut.convertToAttribute(new LangString(value, "en")));
    }

    @Test
    void convertToAttributeReturnsNullWhenValueIsNull() {
        final ToRdfLiteralConverter sut = new ToRdfLiteralConverter(XSD.BOOLEAN);
        assertNull(sut.convertToAxiomValue(null));
    }
}
