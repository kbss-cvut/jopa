/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

public abstract class AbstractFunctionExpression<X> extends AbstractExpression<X> {

    protected AbstractPathExpression argumentExpression;

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


