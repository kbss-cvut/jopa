package cz.cvut.kbss.jopa.oom.converter;

import java.time.Instant;
import java.util.Date;

/**
 * Converts between Java 8 {@link Instant} and {@link Date} used by OntoDriver and the underlying repository access
 * frameworks (RDF4J, Jena, OWLAPI).
 */
public class InstantConverter implements ConverterWrapper<Instant, Date> {

    @Override
    public Date convertToAxiomValue(Instant value) {
        return value != null ? Date.from(value) : null;
    }

    @Override
    public Instant convertToAttribute(Date value) {
        return value != null ? value.toInstant() : null;
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return Date.class.isAssignableFrom(type);
    }
}
