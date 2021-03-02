package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ManagedType;
import cz.cvut.kbss.jopa.model.query.criteria.*;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

import java.util.Collection;
import java.util.List;

public class CriteriaModelImpl<T> extends PathImpl<T> implements CriteriaModel<T> {

    private final UnitOfWorkImpl uow;


    public CriteriaModelImpl(ManagedType<T> managedType, Class<T> type, UnitOfWorkImpl uow) {
        super(managedType,type);
        this.uow = uow;
    }

    @Override
    public Predicate attrEquals(Expression<?> x, Expression<?> y) {
        return null;
    }

    @Override
    public Predicate attrEquals(Expression<?> x, Object y) {
        return null;
    }

    @Override
    public Predicate attrNotEquals(Expression<?> x, Expression<?> y) {
        return null;
    }

    @Override
    public Predicate attrNotEquals(Expression<?> x, Object y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate attrGreaterThan(Expression<? extends Y> x, Expression<? extends Y> y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate attrGreaterThan(Expression<? extends Y> x, Y y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate attrGreaterOrEqual(Expression<? extends Y> x, Expression<? extends Y> y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate attrGreaterOrEqual(Expression<? extends Y> x, Y y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate attrLessThan(Expression<? extends Y> x, Expression<? extends Y> y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate attrLessThan(Expression<? extends Y> x, Y y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate attrLessOrEqual(Expression<? extends Y> x, Expression<? extends Y> y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate attrLessOrEqual(Expression<? extends Y> x, Y y) {
        return null;
    }


}
