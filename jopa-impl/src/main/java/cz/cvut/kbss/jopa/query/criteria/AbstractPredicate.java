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
package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractExpression;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

import java.util.List;

public abstract class AbstractPredicate extends AbstractExpression<Boolean> implements Predicate {

    protected BooleanOperator booleanOperator;

    public AbstractPredicate(BooleanOperator booleanOperator, CriteriaBuilder cb) {
        super(Boolean.class, cb);
        this.booleanOperator = booleanOperator;
    }

    @Override
    public abstract List<Expression<Boolean>> getExpressions();

    @Override
    public BooleanOperator getOperator() {
        return this.booleanOperator;
    }

    @Override
    public abstract void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller);

    protected void negateOperator() {
        if (booleanOperator.equals(BooleanOperator.AND)) {
            booleanOperator = BooleanOperator.OR;
        } else {
            booleanOperator = BooleanOperator.AND;
        }
    }
}
