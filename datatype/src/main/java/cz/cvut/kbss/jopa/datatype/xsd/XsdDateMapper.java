package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.exception.DatatypeMappingException;

import javax.xml.datatype.XMLGregorianCalendar;
import java.time.LocalDate;

/**
 * Maps values of {@link cz.cvut.kbss.jopa.vocabulary.XSD#DATE} to Java {@link LocalDate}.
 * <p>
 * Note that this mapping loses timezone information, if it were present in the literal.
 */
public class XsdDateMapper {

    /**
     * Maps the specified value to {@link LocalDate}.
     *
     * @param value Value to map
     * @return Parsed local date
     */
    public static LocalDate map(String value) {
        try {
            final XMLGregorianCalendar cal = DatatypeFactoryProvider.getFactory().newXMLGregorianCalendar(value);
            return LocalDate.of(cal.getYear(), cal.getMonth(), cal.getDay());
        } catch (IllegalArgumentException e) {
            throw new DatatypeMappingException("Invalid value provided as xsd:date.", e);
        }
    }
}
