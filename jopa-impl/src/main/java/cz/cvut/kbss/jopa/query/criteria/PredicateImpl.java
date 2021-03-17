package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractExpression;

import java.util.List;

//public class PredicateImpl extends AbstractExpression<Boolean> implements Predicate {
//
//    public PredicateImpl(AbstractExpression<Boolean> expression) {
//        super(expression);
//        this.operator = BooleanOperator.AND;
//    }
//
//    public PredicateImpl(AbstractExpression<Boolean> expression, BooleanOperator operator) {
//        super(expression);
//        this.operator = operator;
//    }
//
//    protected BooleanOperator operator;
//
//    @Override
//    public void setExpressionToQuery(StringBuilder query) {
//        return;
//    }
//
//    @Override
//    public List<Expression<Boolean>> getExpressions() {
//        return null;
//    }
//
//    @Override
//    public BooleanOperator getOperator() {
//        return this.operator;
//    }
//
//    @Override
//    public Predicate not() {
//        return null;
//    }
//
//    @Override
//    public boolean isNegated() {
//        return false;
//    }
//}
