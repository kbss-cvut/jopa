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

import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;

public abstract class AbstractComparisonExpression extends AbstractExpression<Boolean> {

    protected final AbstractExpression<?> right;
    protected final AbstractExpression<?> left;

    public AbstractComparisonExpression(AbstractExpression<?> left, AbstractExpression<?> right, CriteriaBuilder cb) {
        super(Boolean.class, cb);
        this.left = left;
        this.right = right;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        this.left.setExpressionToQuery(query, parameterFiller);
        query.append(this.getComparisonOperator());
        this.right.setExpressionToQuery(query, parameterFiller);
    }

    protected abstract String getComparisonOperator();
}


