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

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.criteria.ParameterExpression;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;

import java.util.Objects;

public class ParameterExpressionImpl<T> extends AbstractExpression<T> implements ParameterExpression<T> {
    private String name;

    public ParameterExpressionImpl(Class<T> type, String name, CriteriaBuilder cb) {
        super(type, cb);
        this.name = name;
    }

    public void setNameIfUnnamed(String name) {
        if (this.name == null) this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Integer getPosition() {
        return null;
    }

    @Override
    public Class<T> getParameterType() {
        return type;
    }


    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        query.append(parameterFiller.registerParameter(this));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || !Parameter.class.isAssignableFrom(o.getClass())) {
            return false;
        }
        Parameter<?> that = (Parameter<?>) o;
        return Objects.equals(getName(), that.getName());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getName(), getPosition());
    }
}
