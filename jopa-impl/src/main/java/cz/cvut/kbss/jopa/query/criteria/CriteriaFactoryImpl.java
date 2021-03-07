package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.CriteriaQueryImpl;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.sessions.CriteriaFactory;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class CriteriaFactoryImpl implements CriteriaFactory {

    private final UnitOfWorkImpl uow;

    public CriteriaFactoryImpl(UnitOfWorkImpl uow) {
        this.uow = uow;
    }

    //TODO - query without resultClass
    @Override
    public CriteriaQuery<Object> createQuery() {
        return null;
    }

    @Override
    public <T> CriteriaQuery<T> createQuery(Class<T> resultClass) {
        EntityType<T> entityMetamodel = uow.getMetamodel().entity(resultClass);
        return new CriteriaQueryImpl<T>(new CriteriaQueryHolder<T>(entityMetamodel, resultClass),uow.getMetamodel());
    }

    @Override
    public Predicate and(Expression<Boolean> x, Expression<Boolean> y) {
        return null;
    }

    @Override
    public Predicate and(Predicate... restrictions) {
        return null;
    }

    @Override
    public Predicate or(Expression<Boolean> x, Expression<Boolean> y) {
        return null;
    }

    @Override
    public Predicate or(Predicate... restrictions) {
        return null;
    }

    @Override
    public Predicate equals(Expression<?> x, Expression<?> y) {
        return null;
    }

    @Override
    public Predicate equals(Expression<?> x, Object y) {
        return null;
    }

    @Override
    public Predicate notEquals(Expression<?> x, Expression<?> y) {
        return null;
    }

    @Override
    public Predicate notEquals(Expression<?> x, Object y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Expression<? extends Y> y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterThan(Expression<? extends Y> x, Y y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterOrEqual(Expression<? extends Y> x, Expression<? extends Y> y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate greaterOrEqual(Expression<? extends Y> x, Y y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Expression<? extends Y> y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessThan(Expression<? extends Y> x, Y y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessOrEqual(Expression<? extends Y> x, Expression<? extends Y> y) {
        return null;
    }

    @Override
    public <Y extends Comparable<? super Y>> Predicate lessOrEqual(Expression<? extends Y> x, Y y) {
        return null;
    }

}
