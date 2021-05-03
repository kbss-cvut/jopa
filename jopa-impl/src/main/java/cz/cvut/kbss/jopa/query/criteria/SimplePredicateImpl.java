package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractExpression;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;
import java.util.Collections;
import java.util.List;

public class SimplePredicateImpl extends AbstractPredicate{

    protected Expression<Boolean> expression;

    public SimplePredicateImpl(BooleanOperator booleanOperator, Expression<Boolean> expression, CriteriaBuilder cb) {
        super(booleanOperator, cb);
        this.expression = expression;
    }


    public SimplePredicateImpl(Expression<Boolean> expression, CriteriaBuilder cb) {
        super(BooleanOperator.AND, cb);
        this.expression = expression;
    }

    @Override
    public List<Expression<Boolean>> getExpressions(){
        return Collections.emptyList();
    }

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
