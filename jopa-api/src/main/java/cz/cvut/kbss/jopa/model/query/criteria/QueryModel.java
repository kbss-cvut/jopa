package cz.cvut.kbss.jopa.model.query.criteria;

// TODO PRO - QueryModel methods Javadoc
public interface QueryModel<T> {
    <Y> Path<Y> getAttr(String attributeName);
    Predicate attrEquals(Expression<?> x, Expression<?> y);
    Predicate attrNotEquals(Expression<?> x, Expression<?> y);
    <Y extends Comparable<? super Y>> Predicate attrGreaterThan(Expression<? extends Y> x, Expression<? extends Y> y);
    <Y extends Comparable<? super Y>> Predicate attrGreaterThan(Expression<? extends Y> x, Y y);
    <Y extends Comparable<? super Y>> Predicate attrGreaterOrEqual(Expression<? extends Y> x, Expression<? extends Y> y);
    <Y extends Comparable<? super Y>> Predicate attrGreaterOrEqual(Expression<? extends Y> x, Y y);
    <Y extends Comparable<? super Y>> Predicate attrLessThan(Expression<? extends Y> x, Expression<? extends Y> y);
    <Y extends Comparable<? super Y>> Predicate attrLessThan(Expression<? extends Y> x, Y y);
    <Y extends Comparable<? super Y>> Predicate attrLessOrEqual(Expression<? extends Y> x, Expression<? extends Y> y);
    <Y extends Comparable<? super Y>> Predicate attrLessOrEqual(Expression<? extends Y> x, Y y);
}
