package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.From;
import cz.cvut.kbss.jopa.model.query.criteria.Join;
import cz.cvut.kbss.jopa.model.query.criteria.JoinType;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.soql.SoqlConstants;

public class JoinImpl<Z, X> extends AbstractFrom<Z, X> implements Join<Z, X> {

    private final JoinType joinType;

    private final Attribute<? super Z, X> attribute;

    private final AbstractFrom<?, Z> parent;

    private final String targetAlias;

    public JoinImpl(AbstractFrom<?, Z> parent, Attribute<? super Z, X> attribute, JoinType joinType,
                    Metamodel metamodel,
                    CriteriaBuilderImpl cb) {
        super((Class<X>) attribute.getValueJavaType(), null, metamodel, cb);
        this.parent = parent;
        this.attribute = attribute;
        if (joinType != JoinType.INNER) {
            throw new UnsupportedOperationException("Only INNER JOINs are supported at the moment.");
        }
        this.joinType = joinType;
        this.targetAlias = resolveJoinTargetAlias();
    }

    private String resolveJoinTargetAlias() {
        return attribute.getName().toLowerCase() + "_" + cb.nextCounter();
    }

    @Override
    public String getAlias() {
        return targetAlias;
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
        throw new UnsupportedOperationException();
    }

    @Override
    public Join<Z, X> on(Predicate... restrictions) {
        throw new UnsupportedOperationException();
    }

    void generateJoinExpression(StringBuilder query) {
        query.append(SoqlConstants.JOIN)
             .append(' ')
             .append(parent.getAlias())
             .append('.')
             .append(attribute.getName())
             .append(' ')
             .append(targetAlias);
        if (!joins.isEmpty()) {
            joins.forEach(join -> {
                query.append(' ');
                join.generateJoinExpression(query);
            });
        }
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

    @Override
    protected EntityType<?> entityType() {
        return metamodel.entity(attribute.getValueJavaType());
    }
}
