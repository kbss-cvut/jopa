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

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.CollectionAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.SetAttribute;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.query.criteria.Join;
import cz.cvut.kbss.jopa.model.query.criteria.JoinType;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.model.query.criteria.PredicateFactory;
import cz.cvut.kbss.jopa.model.query.criteria.Root;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractPathExpression;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class RootImpl<X> extends AbstractPathExpression<X> implements Root<X> {

    private final List<JoinImpl<X, ?>> joins = new ArrayList<>();

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
            query.append('.').append(getRootVariableName());
        } else {
            query.append(getRootVariableName());
        }
    }

    public void appendJoins(StringBuilder query) {
        joins.forEach(join -> {
            query.append(' ');
            join.generateJoinExpression(query);
        });
    }

    public String getRootVariableName() {
        return type.getSimpleName().toLowerCase();
    }

    @Override
    public Predicate in(Collection<?> values) {
        final EntityType<X> et = getModel();
        return ((PredicateFactory.In) cb.in(getAttr(et.getIdentifier()))).value(values);
    }

    @Override
    public Set<Join<X, ?>> getJoins() {
        return new LinkedHashSet<>(joins);
    }

    @Override
    public <Y> Join<X, Y> join(CollectionAttribute<? super X, Y> collection) {
        return joinImpl((Attribute<? super X, Y>) collection, JoinType.INNER);
    }

    private <Y> Join<X, Y> joinImpl(Attribute<? super X, Y> attribute, JoinType jt) {
        final JoinImpl<X, Y> join = new JoinImpl<>(this, attribute, jt, metamodel, cb);
        joins.add(join);
        return join;
    }

    @Override
    public <Y> Join<X, Y> join(CollectionAttribute<? super X, Y> collection, JoinType jt) {
        return joinImpl((Attribute<? super X, Y>) collection, jt);
    }

    @Override
    public <Y> Join<X, Y> join(SetAttribute<? super X, Y> set) {
        return joinImpl((Attribute<? super X, Y>) set, JoinType.INNER);
    }

    @Override
    public <Y> Join<X, Y> join(SetAttribute<? super X, Y> set, JoinType jt) {
        return joinImpl((Attribute<? super X, Y>) set, jt);
    }

    @Override
    public <Y> Join<X, Y> join(SingularAttribute<? super X, Y> attribute) {
        return joinImpl(attribute, JoinType.INNER);
    }

    @Override
    public <Y> Join<X, Y> join(SingularAttribute<? super X, Y> attribute, JoinType jt) {
        return joinImpl(attribute, jt);
    }

    @Override
    public <X1, Y> Join<X1, Y> join(String attributeName) {
        final Attribute att = getModel().getAttribute(attributeName);
        return joinImpl(att, JoinType.INNER);
    }

    @Override
    public <X1, Y> Join<X1, Y> join(String attributeName, JoinType jt) {
        final Attribute att = getModel().getAttribute(attributeName);
        return joinImpl(att, jt);
    }
}
