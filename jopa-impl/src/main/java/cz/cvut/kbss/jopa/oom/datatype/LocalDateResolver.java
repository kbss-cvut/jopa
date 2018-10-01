package cz.cvut.kbss.jopa.oom.datatype;

import cz.cvut.kbss.jopa.utils.DatatypeTransformer;

import java.time.LocalDate;
import java.util.Date;

/**
 * Converts between Java 8 {@link LocalDateTime} and {@link Date} used by OntoDriver and the underlying repository
 * access frameworks (RDF4J, Jena, OWLAPI).
 */
public class LocalDateResolver extends ValueResolver {

    @Override
    public Date toAxiom(Object value) {
        return DatatypeTransformer.transform(value, Date.class);
    }

    @Override
    public Object fromAxiom(Object value) {
        return DatatypeTransformer.transform(value, LocalDate.class);
    }
}
