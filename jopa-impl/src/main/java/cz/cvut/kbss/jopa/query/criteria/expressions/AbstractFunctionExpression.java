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

import java.util.List;

public abstract class AbstractFunctionExpression<X> extends AbstractExpression<X> {

    protected final List<AbstractPathExpression> argumentExpression;

    public AbstractFunctionExpression(Class<X> type, CriteriaBuilder cb, AbstractPathExpression ...argumentExpression) {
        super(type, cb);
        this.argumentExpression = List.of(argumentExpression);
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        query.append(getFunctionName()).append("(");
        for (int i = 0; i < argumentExpression.size(); i++) {
            argumentExpression.get(i).setExpressionToQuery(query, parameterFiller);
            if (i < argumentExpression.size() - 1) {
                query.append(',');
            }
        }
        query.append(")");
    }

    public abstract String getFunctionName();
}


