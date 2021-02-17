package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;

import java.util.Collection;

public class ExpressionImpl<Y> extends SelectionImpl<Y> implements Expression<Y> {

    protected final ExpressionType expressionType;
    protected final Y value;

    public <X> ExpressionImpl(Attribute<Y, X> attribute, ExpressionType expressionType, Y value) {
        super(attribute);
        this.expressionType = expressionType;
        this.value = value;
    }

    @Override
    public Predicate in(Collection<?> values) {
        return null;
    }

    @Override
    public Predicate in(Expression<?> values) {
        return null;
    }

    @Override
    public Predicate and(Expression<Boolean> x, Expression<Boolean> y) {
        return null;
    }

    @Override
    public Predicate and(Predicate... restrictions) {
        return null;
    }

    @Override
    public Predicate or(Expression<Boolean> x, Expression<Boolean> y) {
        return null;
    }

    @Override
    public Predicate or(Predicate... restrictions) {
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

    protected enum ExpressionType{
        EQUALS,
        NOTEQUALS,
        LESSTHAN,
    }
}


