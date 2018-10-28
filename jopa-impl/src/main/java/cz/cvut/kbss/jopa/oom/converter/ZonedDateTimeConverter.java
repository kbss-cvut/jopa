package cz.cvut.kbss.jopa.oom.converter;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;

/**
 * Converts between Java 8 {@link ZonedDateTime} and {@link Date} used by OntoDriver and the underlying repository
 * access frameworks (RDF4J, Jena, OWLAPI).
 */
public class ZonedDateTimeConverter implements ConverterWrapper<ZonedDateTime, Date> {

    @Override
    public Date convertToAxiomValue(ZonedDateTime value) {
        return value != null ? Date.from(value.toInstant()) : null;
    }

    @Override
    public ZonedDateTime convertToAttribute(Date value) {
        return value != null ? value.toInstant().atZone(ZoneId.systemDefault()) : null;
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return Date.class.isAssignableFrom(type);
    }
}
