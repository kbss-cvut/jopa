package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.CollectionAttribute;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.SetAttribute;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.From;
import cz.cvut.kbss.jopa.model.query.criteria.Join;
import cz.cvut.kbss.jopa.model.query.criteria.JoinType;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractPathExpression;

import java.util.Set;

public class JoinImpl<Z, X> extends AbstractPathExpression<X> implements Join<Z, X> {

    private final JoinType joinType;

    private final Attribute<? super Z, X> attribute;

    private final RootImpl<Z> parent;

    private final String targetAlias;

    public JoinImpl(RootImpl<Z> parent, Attribute<? super Z, X> attribute, JoinType joinType, Metamodel metamodel,
                    CriteriaBuilderImpl cb) {
        super((Class<X>) attribute.getValueJavaType(), null, metamodel, cb);
        this.parent = parent;
        this.attribute = attribute;
        this.joinType = joinType;
        this.targetAlias = resolveJoinTargetAlias();
    }

    private String resolveJoinTargetAlias() {
        return attribute.getName().toLowerCase() + "_" + cb.nextCounter();
    }

    @Override
    public Attribute<? super Z, ?> getAttribute() {
        return attribute;
    }

    @Override
    public JoinType getJoinType() {
        return joinType;
    }

    @Override
    public Predicate getOn() {
        return null;
    }

    @Override
    public From<?, Z> getParent() {
        return parent;
    }

    @Override
    public Join<Z, X> on(Expression<Boolean> restriction) {
        return null;
    }

    @Override
    public Join<Z, X> on(Predicate... restrictions) {
        return null;
    }

    @Override
    public Set<Join<X, ?>> getJoins() {
        return Set.of();
    }

    @Override
    public <Y> Join<X, Y> join(CollectionAttribute<? super X, Y> collection) {
        return null;
    }

    @Override
    public <Y> Join<X, Y> join(CollectionAttribute<? super X, Y> collection, JoinType jt) {
        return null;
    }

    @Override
    public <Y> Join<X, Y> join(SetAttribute<? super X, Y> set) {
        return null;
    }

    @Override
    public <Y> Join<X, Y> join(SetAttribute<? super X, Y> set, JoinType jt) {
        return null;
    }

    @Override
    public <Y> Join<X, Y> join(SingularAttribute<? super X, Y> attribute) {
        return null;
    }

    @Override
    public <Y> Join<X, Y> join(SingularAttribute<? super X, Y> attribute, JoinType jt) {
        return null;
    }

    @Override
    public <X1, Y> Join<X1, Y> join(String attributeName) {
        return null;
    }

    @Override
    public <X1, Y> Join<X1, Y> join(String attributeName, JoinType jt) {
        return null;
    }

    void generateJoinExpression(StringBuilder query) {
        query.append("JOIN ")
             .append(parent.getRootVariableName())
             .append('.')
             .append(attribute.getName())
             .append(' ')
             .append(targetAlias);
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        if (this.pathSource != null) {
            this.pathSource.setExpressionToQuery(query, parameterFiller);
            query.append('.').append(targetAlias);
        } else {
            query.append(targetAlias);
        }
    }
}
