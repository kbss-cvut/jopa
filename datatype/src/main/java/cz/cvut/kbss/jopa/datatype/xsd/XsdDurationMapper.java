package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.DatatypeMappingException;

import java.time.Duration;
import java.time.Period;
import java.time.format.DateTimeParseException;

/**
 * Maps {@link cz.cvut.kbss.jopa.vocabulary.XSD#DURATION} values to Java {@link Duration}.
 */
public class XsdDurationMapper {

    /**
     * Maps the specified value to {@link Duration}.
     *
     * @param value Value to map
     * @return Parsed duration
     */
    public static Object map(String value) {
        try {
            return Duration.parse(value);
        } catch (DateTimeParseException e) {
            try {
                return Period.parse(value);
            } catch (DateTimeParseException e2) {
                throw new DatatypeMappingException("Unable to resolve value of xsd:duration " + value + ". " +
                        "Maybe it is too large for java.time.Duration, but contains time preventing it from being parsed to java.time.Period.", e);
            }
        }
    }
}
