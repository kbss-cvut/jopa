package cz.cvut.kbss.jopa.model.query.criteria;

import java.util.Collection;

//TODO PRO - Javadoc
public interface Expression<X> extends Selection<X>{
    Predicate in(Collection<?> values);
    Predicate in(Expression<?> values);
    Predicate and(Expression<Boolean> x, Expression<Boolean> y);
    Predicate and(Predicate... restrictions);
    Predicate or(Expression<Boolean> x, Expression<Boolean> y);
    Predicate or(Predicate... restrictions);
    Predicate not();
    boolean isNegated();
}
