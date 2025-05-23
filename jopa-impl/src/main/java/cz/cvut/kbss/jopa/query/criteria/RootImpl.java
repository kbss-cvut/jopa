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
package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.criteria.Root;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractPathExpression;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;

public class RootImpl<X> extends AbstractPathExpression<X> implements Root<X> {

    public RootImpl(Metamodel metamodel, AbstractPathExpression<X> expression, Class<X> type, CriteriaBuilder cb) {
        super(type, expression, metamodel, cb);
    }

    @Override
    public EntityType<X> getModel() {
        return metamodel.entity(type);
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        if (this.pathSource != null){
            this.pathSource.setExpressionToQuery(query, parameterFiller);
            query.append('.').append(type.getSimpleName().toLowerCase());
        } else {
            query.append(type.getSimpleName().toLowerCase());
        }
    }
}
