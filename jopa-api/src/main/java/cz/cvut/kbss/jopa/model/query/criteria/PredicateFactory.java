package cz.cvut.kbss.jopa.model.query.criteria;

// TODO PRO - Javadoc
public interface PredicateFactory {
    /**
     * Create a predicate for testing the arguments for equality.
     * @param x expression
     * @param y expression
     * @return equality predicate
     */
    Predicate attrEquals(Expression<?> x, Expression<?> y);

    /**
     * Create a predicate for testing the arguments for equality.
     * @param x expression
     * @param y object
     * @return equality predicate
     */
    Predicate attrEquals(Expression<?> x, Object y);

    /**
     * Create a predicate for testing the arguments for inequality.
     * @param x expression
     * @param y expression
     * @return inequality predicate
     */
    Predicate attrNotEquals(Expression<?> x, Expression<?> y);

    /**
     * Create a predicate for testing the arguments for inequality.
     * @param x expression
     * @param y object
     * @return inequality predicate
     */
    Predicate attrNotEquals(Expression<?> x, Object y);

    <Y extends Comparable<? super Y>> Predicate attrGreaterThan(Expression<? extends Y> x, Expression<? extends Y> y);
    <Y extends Comparable<? super Y>> Predicate attrGreaterThan(Expression<? extends Y> x, Y y);
    <Y extends Comparable<? super Y>> Predicate attrGreaterOrEqual(Expression<? extends Y> x, Expression<? extends Y> y);
    <Y extends Comparable<? super Y>> Predicate attrGreaterOrEqual(Expression<? extends Y> x, Y y);
    <Y extends Comparable<? super Y>> Predicate attrLessThan(Expression<? extends Y> x, Expression<? extends Y> y);
    <Y extends Comparable<? super Y>> Predicate attrLessThan(Expression<? extends Y> x, Y y);
    <Y extends Comparable<? super Y>> Predicate attrLessOrEqual(Expression<? extends Y> x, Expression<? extends Y> y);
    <Y extends Comparable<? super Y>> Predicate attrLessOrEqual(Expression<? extends Y> x, Y y);
}
