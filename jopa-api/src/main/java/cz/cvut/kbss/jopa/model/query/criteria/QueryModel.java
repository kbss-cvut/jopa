package cz.cvut.kbss.jopa.model.query.criteria;

// TODO PRO - QueryModel methods Javadoc
public interface QueryModel<T> {
    Predicate equals(Expression<?> x, Expression<?> y);
    Predicate notEquals(Expression<?> x, Expression<?> y);
    <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Expression<? extends Y> y);
    <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Y y);
    <Y extends Comparable<? super Y>> Predicate greaterThanOrEqualTo(Expression<? extends Y> x, Expression<? extends Y> y);
    <Y extends Comparable<? super Y>> Predicate greaterThanOrEqualTo(Expression<? extends Y> x, Y y);
    <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Expression<? extends Y> y);
    <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Y y);
    <Y extends Comparable<? super Y>> Predicate lessThanOrEqualTo(Expression<? extends Y> x, Expression<? extends Y> y);
    <Y extends Comparable<? super Y>> Predicate lessThanOrEqualTo(Expression<? extends Y> x, Y y);
}
