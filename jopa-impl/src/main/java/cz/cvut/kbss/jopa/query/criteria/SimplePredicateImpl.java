package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractComparisonExpression;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractExpression;
import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class SimplePredicateImpl extends AbstractPredicate{

    protected Expression<Boolean> expression;

    public SimplePredicateImpl(BooleanOperator booleanOperator, Expression<Boolean> expression, CriteriaFactory factory) {
        super(booleanOperator, factory);
        this.expression = expression;
    }


    public SimplePredicateImpl(Expression<Boolean> expression, CriteriaFactory factory) {
        super(BooleanOperator.AND, factory);
        this.expression = expression;
    }

    @Override
    public List<Expression<Boolean>> getExpressions(){
//        return new ArrayList<Expression<Boolean>>(expression);
        return null;
    };

    @Override
    public BooleanOperator getOperator() {
        return this.booleanOperator;
    }

    @Override
    public Predicate not() {
        ((AbstractExpression)expression).negate();
        super.negate();
        return this;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        AbstractExpression abstractExpression = (AbstractExpression) expression;
        abstractExpression.setExpressionToQuery(query, parameterFiller);
    }

    @Override
    public void negate(){
        this.not();
    }
}
