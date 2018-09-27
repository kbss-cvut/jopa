package cz.cvut.kbss.jopa.oom.datatype;

import cz.cvut.kbss.jopa.utils.DatatypeTransformer;

import java.time.LocalDate;
import java.util.Date;

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
