package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;

import java.util.List;

public class PredicateImpl extends ExpressionImpl<Boolean> implements Predicate {
    public PredicateImpl(Class<Boolean> type, ExpressionImpl expression) {
        super(type, expression);
    }

    @Override
    public String getString() {
        return expression.getString();
    }

    @Override
    public List<Expression<Boolean>> getExpressions() {
        return null;
    }

    @Override
    public BooleanOperator getOperator() {
        return null;
    }

    @Override
    public Predicate not() {
        return null;
    }

    @Override
    public boolean isNegated() {
        return false;
    }
}
