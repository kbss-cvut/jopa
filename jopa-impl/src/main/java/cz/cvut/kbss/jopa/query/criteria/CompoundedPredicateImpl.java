package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractExpression;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class CompoundedPredicateImpl extends AbstractPredicate{

    protected List<Expression<Boolean>> expressions;

    public CompoundedPredicateImpl(BooleanOperator booleanOperator, List<Expression<Boolean>> expressions, CriteriaBuilder cb) {
        super(booleanOperator, cb);
        this.expressions = expressions;
    }

    @Override
    public List<Expression<Boolean>> getExpressions(){
        return expressions;
    }

    @Override
    public BooleanOperator getOperator() {
        return this.booleanOperator;
    }

    @Override
    public Predicate not() {
        for (Expression<Boolean> expression:expressions) {
            AbstractExpression abstractExpression = (AbstractExpression) expression;
            abstractExpression.negate();
        }
        super.negate();
        super.negateOperator();
        return this;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        for (int i = 0; i < expressions.size(); i++) {
            AbstractExpression expression = (AbstractExpression) expressions.get(i);
            if (expression instanceof CompoundedPredicateImpl){
                query.append("(");
                expression.setExpressionToQuery(query,parameterFiller);
                query.append(")");
            } else {
                expression.setExpressionToQuery(query,parameterFiller);
            }

            if(i < (expressions.size()-1)) query.append(" " + this.getOperator().toString() + " ");
        }
    }

    @Override
    public void negate(){
        this.not();
    }
}
