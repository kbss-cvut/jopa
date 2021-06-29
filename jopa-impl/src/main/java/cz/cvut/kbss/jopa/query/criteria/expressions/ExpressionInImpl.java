package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.sessions.PredicateFactory;
import cz.cvut.kbss.jopa.query.criteria.AbstractPredicate;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

import java.util.ArrayList;
import java.util.List;

public class ExpressionInImpl<Y> extends AbstractPredicate implements PredicateFactory.In<Y> {

    private Expression<Y> expression;
    private List<Expression<? extends Y>> values;

    public ExpressionInImpl(Expression<? extends Y> expression, CriteriaBuilder cb) {
        super(BooleanOperator.AND, cb);
        this.expression = (Expression<Y>) expression;
    }

    @Override
    public Expression<Y> getExpression() {
        return expression;
    }

    @Override
    public PredicateFactory.In<Y> value(Y value) {
        if (values == null) values = new ArrayList<>();
        values.add(super.cb.literal(value));
        return this;
    }

    @Override
    public PredicateFactory.In<Y> value(Expression<? extends Y> value) {
        if (values == null) values = new ArrayList<>();
        values.add(value);
        return this;
    }


    @Override
    public Predicate not() {
        super.negate();
        return this;
    }

    @Override
    public List<Expression<Boolean>> getExpressions() {
        List<Expression<Boolean>> expressions = new ArrayList<>();
        expressions.add(this);
        return expressions;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        ((AbstractExpression) expression).setExpressionToQuery(query, parameterFiller);
        query.append(negated ? " NOT IN(" : " IN(");
        for (int i = 0; i < values.size(); i++) {
            ((AbstractExpression) values.get(i)).setExpressionToQuery(query, parameterFiller);
            if (values.size() > 1 && (i + 1) != values.size()){
                query.append(", ");
            }
        }
        query.append(")");
    }
}


