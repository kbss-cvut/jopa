package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.DatatypeMappingException;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

/**
 * Maps a value of {@link cz.cvut.kbss.jopa.vocabulary.XSD#TIME} to Java {@link OffsetTime}.
 * <p>
 * Note that if the time does not specify offset information, system local is used.
 */
public class XsdTimeMapper {

    private static final ZoneOffset LOCAL_OFFSET = ZoneId.systemDefault().getRules().getOffset(LocalDateTime.now());

    /**
     * Maps the specified value to {@link OffsetTime}.
     *
     * @param value Value to map
     * @return Parsed offset time
     */
    public static OffsetTime map(String value) {
        try {
            return OffsetTime.parse(value, DateTimeFormatter.ISO_OFFSET_TIME);
        } catch (DateTimeParseException e) {
            try {
                return LocalTime.parse(value).atOffset(LOCAL_OFFSET);
            } catch (DateTimeParseException e2) {
                throw new DatatypeMappingException("Invalid value provided as xsd:time.", e2);
            }
        }
    }
}
