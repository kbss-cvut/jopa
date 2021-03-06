package cz.cvut.kbss.jopa.model.query.criteria;

import java.util.Collection;

public interface Expression<X> extends Selection<X>{

    /**
     * Create a predicate to test whether the expression is a member of the collection.
     * @param values collection of values to be tested against
     * @return predicate testing for membership
     */
    Predicate in(Collection<?> values);

    /**
     * Create a predicate to test whether the expression is a member of the argument list.
     * @param values expressions to be tested against
     * @return predicate testing for membership
     */
    Predicate in(Expression<?>... values);

    /**
     * Create a predicate to test whether the expression is a member of the argument list.
     * @param values values to be tested against
     * @return predicate testing for membership
     */
    Predicate in(Object... values);

    /**
     * Create a predicate to test whether the expression is not null.
     * @return predicate testing whether the expression is not null
     */
    Predicate isNotNull();

    /**
     * Create a predicate to test whether the expression is null.
     * @return predicate testing whether the expression is null
     */
    Predicate isNull();

}
