package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.model.query.criteria.PredicateFactory;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.query.criteria.SelectionImpl;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

/**
 * Parent of all other types of expressions. It offers its children the implementation of methods for IN expression as prescribed by the Expression interface.
 * Prescribes an abstract method for expressing an expression to string builder representing the query.
 */
abstract public class AbstractExpression<Y> extends SelectionImpl<Y> implements Expression<Y> {

    protected final CriteriaBuilder cb;

    protected boolean negated;

    public AbstractExpression(Class<Y> type, CriteriaBuilder cb) {
        super(type);
        this.cb = cb;
        negated = false;
    }

    //TODO - BAKALARKA - KONZULTACIA
    // da sa Collection<?> spracovat lepsie?
    @Override
    public Predicate in(Collection<?> values) {
        PredicateFactory.In<Y> predicate = cb.in(this);
        for (Object value : values) {
            if (value instanceof AbstractExpression) {
                predicate.value((Expression<? extends Y>) value);
            } else {
                predicate.value(cb.literal((Y) value));
            }
        }
        return predicate;
    }

    //TODO - BAKALARKA - KONZULTACIA
    // da sa spracovat lepsie?
    @Override
    public Predicate in(Expression<?>... values) {
        PredicateFactory.In<Y> predicate = cb.in(this);
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


