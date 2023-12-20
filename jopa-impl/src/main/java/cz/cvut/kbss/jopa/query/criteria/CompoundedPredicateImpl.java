/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractExpression;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

import java.util.List;

public class CompoundedPredicateImpl extends AbstractPredicate {

    protected List<Expression<Boolean>> expressions;

    public CompoundedPredicateImpl(BooleanOperator booleanOperator, List<Expression<Boolean>> expressions,
                                   CriteriaBuilder cb) {
        super(booleanOperator, cb);
        this.expressions = expressions;
    }

    @Override
    public List<Expression<Boolean>> getExpressions() {
        return expressions;
    }

    @Override
    public BooleanOperator getOperator() {
        return this.booleanOperator;
    }

    @Override
    public Predicate not() {
        for (Expression<Boolean> expression : expressions) {
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
            if (expression instanceof CompoundedPredicateImpl) {
                query.append("(");
                expression.setExpressionToQuery(query, parameterFiller);
                query.append(")");
            } else {
                expression.setExpressionToQuery(query, parameterFiller);
            }

            if (i < (expressions.size() - 1)) {
                query.append(" ").append(getOperator()).append(" ");
            }
        }
    }

    @Override
    public void negate() {
        this.not();
    }
}
