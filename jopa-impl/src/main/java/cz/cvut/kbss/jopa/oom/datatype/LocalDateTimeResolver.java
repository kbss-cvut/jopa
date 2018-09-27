package cz.cvut.kbss.jopa.oom.datatype;

import cz.cvut.kbss.jopa.utils.DatatypeTransformer;

import java.time.LocalDateTime;

public class LocalDateTimeResolver extends LocalDateResolver {

    @Override
    public Object fromAxiom(Object value) {
        return DatatypeTransformer.transform(value, LocalDateTime.class);
    }
}
