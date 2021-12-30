package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.exception.DatatypeMappingException;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;

import java.time.*;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalAmount;
import java.util.Objects;

import static cz.cvut.kbss.jopa.datatype.DateTimeUtil.toDateTime;
import static cz.cvut.kbss.jopa.datatype.DateTimeUtil.toTime;

/**
 * Maps temporal values to their corresponding XSD datatype values.
 */
public final class XsdTemporalMapper {

    private XsdTemporalMapper() {
        throw new AssertionError();
    }

    /**
     * Maps the specified temporal value to a corresponding XSD literal.
     * <p>
     * Note that only a subset of {@link TemporalAccessor} subclasses is supported. This subset corresponds to the generic
     * date/time handling types declared in the {@code java.time} package (e.g., {@link LocalDateTime}, {@link OffsetDateTime} etc.).
     *
     * @param value Value to map
     * @return RDF literal with correct lexical form and datatype
     * @throws DatatypeMappingException When the temporal value is not supported
     */
    public static Literal map(TemporalAccessor value) {
        if (value instanceof OffsetDateTime) {
            return Literal.from(value.toString(), XSD.DATETIME);
        } else if (value instanceof LocalDateTime) {
            return Literal.from(toDateTime((LocalDateTime) value).toString(), XSD.DATETIME);
        } else if (value instanceof Instant) {
            return Literal.from(toDateTime((Instant) value).toString(), XSD.DATETIME);
        } else if (value instanceof ZonedDateTime) {
            return Literal.from(((ZonedDateTime) value).toOffsetDateTime().toString(), XSD.DATETIME);
        } else if (value instanceof OffsetTime) {
            return Literal.from(value.toString(), XSD.TIME);
        } else if (value instanceof LocalTime) {
            return Literal.from(toTime((LocalTime) value).toString(), XSD.TIME);
        } else if (value instanceof LocalDate) {
            return Literal.from(value.toString(), XSD.DATE);
        }
        throw new DatatypeMappingException("Unsupported temporal accessor value " + value);
    }

    /**
     * Maps the specified temporal amount value to a corresponding XSD duration literal.
     *
     * @param value Value to map
     * @return RDF literal representing XSD duration value
     */
    public static Literal map(TemporalAmount value) {
        return Literal.from(Objects.requireNonNull(value).toString(), XSD.DURATION);
    }
}
