/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

public abstract class AbstractFunctionExpression<X> extends AbstractExpression<X> {

    protected final AbstractPathExpression argumentExpression;

    public AbstractFunctionExpression(Class<X> type, AbstractPathExpression argumentExpression, CriteriaBuilder cb) {
        super(type, cb);
        this.argumentExpression = argumentExpression;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        query.append(getFunctionName()).append("(");
        argumentExpression.setExpressionToQuery(query, parameterFiller);
        query.append(")");
    }

    public abstract String getFunctionName();
}


