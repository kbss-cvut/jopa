package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.CollectionAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.SetAttribute;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.query.criteria.From;
import cz.cvut.kbss.jopa.model.query.criteria.Join;
import cz.cvut.kbss.jopa.model.query.criteria.JoinType;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractPathExpression;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public abstract class AbstractFrom<Z, X> extends AbstractPathExpression<X> implements From<Z, X> {

    protected final List<JoinImpl<X, ?>> joins = new ArrayList<>();

    public AbstractFrom(Class<X> type, AbstractPathExpression pathSource,
                        Metamodel metamodel, CriteriaBuilderImpl cb) {
        super(type, pathSource, metamodel, cb);
    }

    @Override
    public Set<Join<X, ?>> getJoins() {
        return new LinkedHashSet<>(joins);
    }

    @Override
    public <Y> Join<X, Y> join(CollectionAttribute<? super X, Y> collection) {
        return joinImpl((Attribute<? super X, Y>) collection, JoinType.INNER);
    }

    private <Y> Join<X, Y> joinImpl(Attribute<? super X, Y> attribute, JoinType jt) {
        final JoinImpl<X, Y> join = new JoinImpl<>(this, attribute, jt, metamodel, cb);
        joins.add(join);
        return join;
    }

    @Override
    public <Y> Join<X, Y> join(CollectionAttribute<? super X, Y> collection, JoinType jt) {
        return joinImpl((Attribute<? super X, Y>) collection, jt);
    }

    @Override
    public <Y> Join<X, Y> join(SetAttribute<? super X, Y> set) {
        return joinImpl((Attribute<? super X, Y>) set, JoinType.INNER);
    }

    @Override
    public <Y> Join<X, Y> join(SetAttribute<? super X, Y> set, JoinType jt) {
        return joinImpl((Attribute<? super X, Y>) set, jt);
    }

    @Override
    public <Y> Join<X, Y> join(SingularAttribute<? super X, Y> attribute) {
        return joinImpl(attribute, JoinType.INNER);
    }

    @Override
    public <Y> Join<X, Y> join(SingularAttribute<? super X, Y> attribute, JoinType jt) {
        return joinImpl(attribute, jt);
    }

    @Override
    public <X1, Y> Join<X1, Y> join(String attributeName) {
        final Attribute att = entityType().getAttribute(attributeName);
        return joinImpl(att, JoinType.INNER);
    }

    @Override
    public <X1, Y> Join<X1, Y> join(String attributeName, JoinType jt) {
        final Attribute att = entityType().getAttribute(attributeName);
        return joinImpl(att, jt);
    }

    protected abstract EntityType<?> entityType();
}
