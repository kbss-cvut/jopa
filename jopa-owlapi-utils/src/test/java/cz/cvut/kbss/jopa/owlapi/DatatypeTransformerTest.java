package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.vocab.OWL2Datatype;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Date;

import static cz.cvut.kbss.jopa.datatype.DateTimeUtil.SYSTEM_OFFSET;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class DatatypeTransformerTest {

    private static final OWLDataFactory DATA_FACTORY = new OWLDataFactoryImpl();

    @Test
    void transformOwlLiteralToObjectReturnsOntoDriverLangStringForRdfLangString() {
        final OWLLiteral literal = DATA_FACTORY.getOWLLiteral("test", "en");
        final Object result = DatatypeTransformer.transform(literal);
        assertThat(result, instanceOf(LangString.class));
        final LangString lsResult = (LangString) result;
        assertEquals(literal.getLiteral(), lsResult.getValue());
        assertTrue(lsResult.getLanguage().isPresent());
        assertEquals(literal.getLang(), lsResult.getLanguage().get());
    }

    @Test
    void transformOwlLiteralToObjectReturnsStringForSimpleLiteral() {
        final OWLLiteral literal = DATA_FACTORY.getOWLLiteral("test");
        final Object result = DatatypeTransformer.transform(literal);
        assertThat(result, instanceOf(String.class));
        assertEquals(literal.getLiteral(), result);
    }

    @Test
    void transformObjectToOwlLiteralReturnsRdfLangStringForOntoDriverLangStringWithLanguage() {
        final LangString ls = new LangString("test", "en");
        final OWLLiteral result = DatatypeTransformer.transform(ls, null);
        assertEquals(OWL2Datatype.RDF_LANG_STRING.getDatatype(DATA_FACTORY), result.getDatatype());
        assertEquals(ls.getValue(), result.getLiteral());
        assertEquals(ls.getLanguage().get(), result.getLang());
    }

    @Test
    void transformObjectToOwlLiteralReturnsSimpleLiteralForOntoDriverLangStringWithoutLanguage() {
        final LangString ls = new LangString("test");
        final OWLLiteral result = DatatypeTransformer.transform(ls, null);
        assertEquals(OWL2Datatype.XSD_STRING.getDatatype(DATA_FACTORY), result.getDatatype());
        assertEquals(ls.getValue(), result.getLiteral());
        assertFalse(result.hasLang());
        assertTrue(result.getLang().isEmpty());
    }

    @Test
    void transformObjectToOwlLiteralSupportsMappingOntoLiteralWithLexicalFormAndDatatype() {
        final Literal ontoLiteral = new Literal("P1Y", XSD.DURATION);
        final OWLLiteral result = DatatypeTransformer.transform(ontoLiteral, null);
        assertEquals(ontoLiteral.getLexicalForm(), result.getLiteral());
        assertEquals(XSD.DURATION, result.getDatatype().toStringID());
    }

    @Test
    void transformOwlLiteralToObjectReturnsOntoDriverLiteralForUnknownDatatype() {
        final OWLLiteral literal = DATA_FACTORY.getOWLLiteral("P1Y", DATA_FACTORY.getOWLDatatype(XSD.DURATION));
        final Object result = DatatypeTransformer.transform(literal);
        assertThat(result, instanceOf(cz.cvut.kbss.ontodriver.model.Literal.class));
        final cz.cvut.kbss.ontodriver.model.Literal literalResult = (cz.cvut.kbss.ontodriver.model.Literal) result;
        assertEquals(literal.getLiteral(), literalResult.getLexicalForm());
        assertEquals(literal.getDatatype().toStringID(), literalResult.getDatatype());
    }

    @Test
    void transformObjectToOwlLiteralReturnsXsdDateTimeAtUTCOffsetForDate() {
        final Instant instant = Instant.now().truncatedTo(ChronoUnit.MILLIS);
        final Date value = Date.from(instant);
        final OWLLiteral result = DatatypeTransformer.transform(value, null);
        assertEquals(instant.atOffset(ZoneOffset.UTC).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                result.getLiteral());
        assertEquals(XSD.DATETIME, result.getDatatype().toStringID());
    }

    @Test
    void transformObjectToOwlLiteralReturnsXsdDateTimeAtSystemOffsetForLocalDateTime() {
        final LocalDateTime value = LocalDateTime.now();
        final OWLLiteral result = DatatypeTransformer.transform(value, null);
        assertEquals(value.atOffset(SYSTEM_OFFSET).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME), result.getLiteral());
        assertEquals(XSD.DATETIME, result.getDatatype().toStringID());
    }

    @Test
    void transformObjectToOwlLiteralReturnsXsdTimeAtSystemOffsetForLocalTime() {
        final LocalTime value = LocalTime.now();
        final OWLLiteral result = DatatypeTransformer.transform(value, null);
        assertEquals(value.atOffset(SYSTEM_OFFSET).format(DateTimeFormatter.ISO_OFFSET_TIME), result.getLiteral());
        assertEquals(XSD.TIME, result.getDatatype().toStringID());
    }

    @Test
    void transformObjectToOwlLiteralReturnsXsdDateForLocalDate() {
        final LocalDate value = LocalDate.now();
        final OWLLiteral result = DatatypeTransformer.transform(value, null);
        assertEquals(value.format(DateTimeFormatter.ISO_DATE), result.getLiteral());
        assertEquals(XSD.DATE, result.getDatatype().toStringID());
    }

    @Test
    void transformObjectToOwlLiteralReturnsXsdDurationForJavaDuration() {
        final Duration value = Duration.ofSeconds(158);
        final OWLLiteral result = DatatypeTransformer.transform(value, null);
        assertEquals(value.toString(), result.getLiteral());
        assertEquals(XSD.DURATION, result.getDatatype().toStringID());
    }
}
