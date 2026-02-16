/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.query.soql.SoqlConstants;

public class LangMatchesFunction extends AbstractExpression<Boolean> {

    private final AbstractExpression<String> value;
    private final AbstractExpression<String> range;

    public LangMatchesFunction(CriteriaBuilder cb, AbstractExpression<String> value, AbstractExpression<String> range) {
        super(Boolean.class, cb);
        this.value = value;
        this.range = range;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        query.append(SoqlConstants.Functions.LANG_MATCHES)
                .append('(');
        value.setExpressionToQuery(query, parameterFiller);
        query.append(", ");
        range.setExpressionToQuery(query, parameterFiller);
        query.append(')');
    }
}
