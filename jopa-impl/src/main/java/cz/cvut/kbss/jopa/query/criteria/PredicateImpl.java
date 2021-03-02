package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;

public class PredicateImpl extends ExpressionImpl<Boolean> implements Predicate {
    public PredicateImpl(Class<Boolean> type, ExpressionImpl expression) {
        super(type, expression);
    }

    @Override
    public BooleanOperator getOperator() {
        return null;
    }

//    final protected Predicate.BooleanOperator booleanOperator;
//
//    public <X> PredicateImpl(Attribute<Boolean, X> attribute, ExpressionType expressionType, Boolean value, BooleanOperator booleanOperator) {
//        super(attribute, expressionType, value);
//        this.booleanOperator = booleanOperator;
//    }
//
//    @Override
//    public BooleanOperator getOperator() {
//        return null;
//    }
}
