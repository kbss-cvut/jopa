package cz.cvut.kbss.jopa.oom.converter.datetime;

import cz.cvut.kbss.jopa.exception.IncompatibleDatatypeException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;

/**
 * Converts between RDF literals representing xsd:dateTime and {@link Date} instances.
 *
 * Note that when transforming to the RDF literal, UTC time zone is assumed to provide consistent results.
 */
public class DateConverter implements ConverterWrapper<Date, Literal> {

    private static final DatatypeFactory DATATYPE_FACTORY = initDatatypeFactory();

    private static DatatypeFactory initDatatypeFactory() {
        try {
            return DatatypeFactory.newInstance();
        } catch (DatatypeConfigurationException e) {
            throw new OWLPersistenceException("Fatal error, unable to resolve DatatypeFactory.", e);
        }
    }

    @Override
    public Literal convertToAxiomValue(Date value) {
        // TODO Use InstantConverter once it is implemented
        return new Literal(ZonedDateTime.ofInstant(value.toInstant(), ZoneOffset.UTC).format(DateTimeFormatter.ISO_INSTANT), XSD.DATETIME);
    }

    @Override
    public Date convertToAttribute(Literal value) {
        if (!XSD.DATETIME.equals(value.getDatatype())) {
            throw new IncompatibleDatatypeException("Literal " + value + " cannot be mapped to " + Date.class);
        }
        final XMLGregorianCalendar calendar = DATATYPE_FACTORY.newXMLGregorianCalendar(value.getLexicalForm());
        return calendar.toGregorianCalendar().getTime();
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return Literal.class.isAssignableFrom(type);
    }
}
