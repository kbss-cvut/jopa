package cz.cvut.kbss.jopa.model.query.criteria;

public interface PredicateFactory {

    /**
     * Create a conjunction of the given boolean expressions.
     * @param x boolean expression
     * @param y boolean expression
     * @return and predicate
     */
    Predicate and(Expression<Boolean> x, Expression<Boolean> y);

    /**
     * Create a conjunction of the given restriction predicates. A conjunction of zero predicates is true.
     * @param restrictions zero or more restriction predicates
     * @return and predicate
     */
    Predicate and(Predicate... restrictions);

    /**
     * Create a disjunction of the given boolean expressions.
     * @param x boolean expression
     * @param y boolean expression
     * @return or predicate
     */
    Predicate or(Expression<Boolean> x, Expression<Boolean> y);

    /**
     * Create a disjunction of the given restriction predicates. A disjunction of zero predicates is false.
     * @param restrictions zero or more restriction predicates
     * @return or predicate
     */
    Predicate or(Predicate... restrictions);

    /**
     * Create a predicate for testing the arguments for equality.
     * @param x expression
     * @param y expression
     * @return equality predicate
     */
    Predicate equals(Expression<?> x, Expression<?> y);

    /**
     * Create a predicate for testing the arguments for equality.
     * @param x expression
     * @param y object
     * @return equality predicate
     */
    Predicate equals(Expression<?> x, Object y);

    /**
     * Create a predicate for testing the arguments for inequality.
     * @param x expression
     * @param y expression
     * @return inequality predicate
     */
    Predicate notEquals(Expression<?> x, Expression<?> y);

    /**
     * Create a predicate for testing the arguments for inequality.
     * @param x expression
     * @param y object
     * @return inequality predicate
     */
    Predicate notEquals(Expression<?> x, Object y);

    /**
     * Create a predicate for testing whether the first argument is greater than the second.
     * @param x expression
     * @param y expression
     * @return greaterThan predicate
     */
    <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Expression<? extends Y> y);

    /**
     * Create a predicate for testing whether the first argument is greater than the second.
     * @param x expression
     * @param y value
     * @return greaterThan predicate
     */
    <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Y y);

    /**
     * Create a predicate for testing whether the first argument is greater than or equal to the second.
     * @param x expression
     * @param y expression
     * @return greaterThanOrEqual predicate
     */
    <Y extends Comparable<? super Y>> Predicate greaterOrEqual(Expression<? extends Y> x, Expression<? extends Y> y);

    /**
     * Create a predicate for testing whether the first argument is greater than or equal to the second.
     * @param x expression
     * @param y value
     * @return greaterThanOrEqual predicate
     */
    <Y extends Comparable<? super Y>> Predicate greaterOrEqual(Expression<? extends Y> x, Y y);

    /**
     * Create a predicate for testing whether the first argument is less than the second.
     * @param x expression
     * @param y expression
     * @return lessThan predicate
     */
    <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Expression<? extends Y> y);

    /**
     * Create a predicate for testing whether the first argument is less than the second.
     * @param x expression
     * @param y value
     * @return lessThan predicate
     */
    <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Y y);

    /**
     * Create a predicate for testing whether the first argument is less than or equal to the second.
     * @param x expression
     * @param y expression
     * @return lessThanOrEqual predicate
     */
    <Y extends Comparable<? super Y>> Predicate lessOrEqual(Expression<? extends Y> x, Expression<? extends Y> y);

    /**
     * Create a predicate for testing whether the first argument is less than or equal to the second.
     * @param x expression
     * @param y value
     * @return lessThanOrEqual predicate
     */
    <Y extends Comparable<? super Y>> Predicate lessOrEqual(Expression<? extends Y> x, Y y);
}
