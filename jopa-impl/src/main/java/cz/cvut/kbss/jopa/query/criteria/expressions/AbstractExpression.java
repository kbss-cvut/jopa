/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.query.criteria.SelectionImpl;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.model.query.criteria.PredicateFactory;

import java.util.Arrays;
import java.util.Collection;

/**
 * Parent of all other types of expressions.
 * <p>
 * It offers its children the implementation of methods for IN expression as prescribed by the Expression interface.
 * Prescribes an abstract method for expressing an expression to string builder representing the query.
 */
public abstract class AbstractExpression<Y> extends SelectionImpl<Y> implements Expression<Y> {

    protected final CriteriaBuilder cb;

    protected boolean negated;

    public AbstractExpression(Class<Y> type, CriteriaBuilder cb) {
        super(type);
        this.cb = cb;
        negated = false;
    }

    @Override
    public Predicate in(Collection<?> values) {
        PredicateFactory.In<Y> predicate = cb.in(this);
        values.forEach(v -> predicate.value((Y) v));
        return predicate;
    }

    @Override
    public Predicate in(Object... values) {
        return this.in(Arrays.asList(values));
    }

    public abstract void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller);

    public boolean isNegated() {
        return negated;
    }

    public void negate() {
        this.negated = true;
    }
}


