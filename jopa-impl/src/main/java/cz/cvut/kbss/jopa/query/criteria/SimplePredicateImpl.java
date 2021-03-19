package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractExpression;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class SimplePredicateImpl extends AbstractPredicate{

    protected AbstractExpression<Boolean> expression;

    public SimplePredicateImpl(BooleanOperator booleanOperator, AbstractExpression<Boolean> expression) {
        super(booleanOperator);
        this.expression = expression;
    }


    public SimplePredicateImpl(AbstractExpression<Boolean> expression) {
        super(BooleanOperator.AND);
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
        return null;
    }

    @Override
    public boolean isNegated() {
        return false;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query) {
        expression.setExpressionToQuery(query);
    }
}
