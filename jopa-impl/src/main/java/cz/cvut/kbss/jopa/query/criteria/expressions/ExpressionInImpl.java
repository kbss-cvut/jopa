/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.criteria.expressions;

import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.query.criteria.AbstractPredicate;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;
import cz.cvut.kbss.jopa.sessions.PredicateFactory;

import java.util.ArrayList;
import java.util.List;

public class ExpressionInImpl<Y> extends AbstractPredicate implements PredicateFactory.In<Y> {

    private final Expression<Y> expression;
    private final List<Y> values = new ArrayList<>();

    public ExpressionInImpl(Expression<? extends Y> expression, CriteriaBuilder cb) {
        super(BooleanOperator.AND, cb);
        this.expression = (Expression<Y>) expression;
    }

    @Override
    public Expression<Y> getExpression() {
        return expression;
    }

    @Override
    public PredicateFactory.In<Y> value(Y value) {
        values.add(value);
        return this;
    }

    @Override
    public Predicate not() {
        super.negate();
        return this;
    }

    @Override
    public List<Expression<Boolean>> getExpressions() {
        List<Expression<Boolean>> expressions = new ArrayList<>();
        expressions.add(this);
        return expressions;
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        ((AbstractExpression<?>) expression).setExpressionToQuery(query, parameterFiller);
        query.append(negated ? " NOT IN (" : " IN (");
        final AbstractExpression<?> param = (AbstractExpression<?>) cb.literal(values);
        param.setExpressionToQuery(query, parameterFiller);
        query.append(")");
    }
}


