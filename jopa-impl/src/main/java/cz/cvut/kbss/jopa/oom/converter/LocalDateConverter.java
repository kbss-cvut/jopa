package cz.cvut.kbss.jopa.oom.converter;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

/**
 * Converts between Java 8 {@link LocalDate} and {@link Date} used by OntoDriver and the underlying repository access
 * frameworks (RDF4J, Jena, OWLAPI).
 * <p>
 * Note that conversion to entity attribute uses the default system time zone.
 */
public class LocalDateConverter implements ConverterWrapper<LocalDate, Date> {

    @Override
    public Date convertToAxiomValue(LocalDate value) {
        return value != null ? java.sql.Date.valueOf(value) : null;
    }

    @Override
    public LocalDate convertToAttribute(Date value) {
        return value != null ? value.toInstant().atZone(ZoneId.systemDefault()).toLocalDate() : null;
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return Date.class.isAssignableFrom(type);
    }
}
