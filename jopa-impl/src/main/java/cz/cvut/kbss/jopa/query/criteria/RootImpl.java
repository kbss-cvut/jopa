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
package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.model.query.criteria.PredicateFactory;
import cz.cvut.kbss.jopa.model.query.criteria.Root;

import java.util.Collection;

public class RootImpl<X> extends AbstractFrom<X, X> implements Root<X> {

    public RootImpl(Metamodel metamodel, Class<X> type, CriteriaBuilderImpl cb) {
        super(type, null, metamodel, cb);
    }

    @Override
    public EntityType<X> getModel() {
        return metamodel.entity(type);
    }

    @Override
    public void setExpressionToQuery(StringBuilder query, CriteriaParameterFiller parameterFiller) {
        if (this.pathSource != null) {
            this.pathSource.setExpressionToQuery(query, parameterFiller);
            query.append('.').append(getAlias());
        } else {
            query.append(getAlias());
        }
    }

    public void appendJoins(StringBuilder query) {
        joins.forEach(join -> {
            query.append(' ');
            join.generateJoinExpression(query);
        });
    }

    @Override
    public String getAlias() {
        return type.getSimpleName().toLowerCase();
    }

    @Override
    public Predicate in(Collection<?> values) {
        final EntityType<X> et = getModel();
        return ((PredicateFactory.In) cb.in(getAttr(et.getIdentifier()))).value(values);
    }

    @Override
    protected EntityType<X> entityType() {
        return getModel();
    }
}
