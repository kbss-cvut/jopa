package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.model.query.criteria.PredicateFactory;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.query.criteria.SelectionImpl;
import cz.cvut.kbss.jopa.sessions.CriteriaFactory;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

abstract public class AbstractExpression<Y> extends SelectionImpl<Y> implements Expression<Y> {

    protected final CriteriaFactory factory;

    protected boolean negated;

    public AbstractExpression(Class<Y> type, CriteriaFactory factory) {
        super(type);
        this.factory = factory;
        negated = false;
    }

    //TODO - BAKALARKA - KONZULTACIA
    // da sa Collection<?> spracovat lepsie?
    @Override
    public Predicate in(Collection<?> values) {
        PredicateFactory.In<Y> predicate = factory.in(this);
        for (Object value : values) {
            if (value instanceof AbstractExpression) {
                predicate.value((Expression<? extends Y>) value);
            } else {
                predicate.value(factory.literal((Y) value));
            }
        }
        return predicate;
    }

    //TODO - BAKALARKA - KONZULTACIA
    // da sa spracovat lepsie?
    @Override
    public Predicate in(Expression<?>... values) {
        PredicateFactory.In<Y> predicate = factory.in(this);
        for (Expression value : values) {
                predicate.value((Expression<? extends Y>) value);
        }
        return predicate;
    }

    @Override
    public Predicate in(Object... values) {
        return this.in(Arrays.asList(values));
    }

    abstract public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller);

    public boolean isNegated() {
        return negated;
    }

    public void negate() {
        this.negated = true;
    }
}


