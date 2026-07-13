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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.Expression;
import cz.cvut.kbss.jopa.model.query.criteria.Order;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.model.query.criteria.Root;
import cz.cvut.kbss.jopa.model.query.criteria.Selection;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.query.criteria.AbstractPredicate;
import cz.cvut.kbss.jopa.query.criteria.CriteriaBuilderImpl;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.query.criteria.CriteriaQueryHolder;
import cz.cvut.kbss.jopa.query.criteria.RootImpl;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractExpression;
import cz.cvut.kbss.jopa.query.soql.SoqlConstants;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

public class CriteriaQueryImpl<T> implements CriteriaQuery<T> {

    protected final CriteriaQueryHolder<T> query;
    private final Metamodel metamodel;
    private final CriteriaBuilderImpl cb;

    private QueryType queryType = QueryType.SELECT;


    public CriteriaQueryImpl(CriteriaQueryHolder<T> query, Metamodel metamodel, CriteriaBuilderImpl cb) {
        this.query = Objects.requireNonNull(query);
        this.metamodel = metamodel;
        this.cb = cb;
    }

    @Override
    public <X> Root<X> from(Class<X> entityClass) {
        RootImpl<X> root = new RootImpl<>(metamodel, null, entityClass, this.cb);
        query.setRoot(root);
        return root;
    }

    @Override
    public <X> Root<X> from(EntityType<X> entity) {
        RootImpl<X> root = new RootImpl<>(metamodel, null, entity.getJavaType(), this.cb);
        query.setRoot(root);
        return root;
    }

    @Override
    public CriteriaQuery<T> select(Selection<? extends T> selection) {
        query.setSelection(selection);
        this.queryType = QueryType.SELECT;
        return this;
    }

    @Override
    public CriteriaQuery<Boolean> ask() {
        this.queryType = QueryType.ASK;
        query.setSelection(null);
        assert Boolean.class.equals(query.getResultType());
        return (CriteriaQuery<Boolean>) this;
    }

    @Override
    public CriteriaQuery<T> where(Expression<Boolean> expression) {
        query.setWhere(cb.wrapExpressionToPredicateWithRepair(expression));
        return this;
    }

    @Override
    public CriteriaQuery<T> where(Predicate... predicates) {
        query.setWhere(cb.and(predicates));
        return this;
    }

    @Override
    public CriteriaQuery<T> where(List<Predicate> predicates) {
        return this.where(predicates.toArray(new Predicate[0]));
    }

    @Override
    public Class<T> getResultType() {
        return query.getResultType();
    }

    @Override
    public CriteriaQuery<T> distinct(boolean b) {
        query.setDistinct(b);
        return this;
    }

    @Override
    public CriteriaQuery<T> distinct() {
        return this.distinct(true);
    }

    @Override
    public boolean isDistinct() {
        return query.isDistinct();
    }

    @Override
    public Selection<T> getSelection() {
        return (Selection<T>) query.getSelection();
    }

    @Override
    public Predicate getRestriction() {
        return query.getWhere();
    }

    @Override
    public CriteriaQuery<T> orderBy(List<Order> o) {
        if (!o.isEmpty()) {
            query.setOrderBy(o);
        } else {
            query.setOrderBy(null);
        }
        return this;
    }

    @Override
    public CriteriaQuery<T> orderBy(Order... o) {
        if (o != null && o.length > 0) {
            query.setOrderBy(Arrays.asList(o));
        } else {
            query.setOrderBy(null);
        }
        return this;
    }

    @Override
    public List<Order> getOrderList() {
        if (query.getOrderBy() == null) {
            return Collections.emptyList();
        }
        return new ArrayList<>(query.getOrderBy());
    }


    @Override
    public CriteriaQuery<T> groupBy(Expression<?>... grouping) {
        if (grouping != null && grouping.length > 0) {
            query.setGroupBy(Arrays.asList(grouping));
        } else {
            query.setGroupBy(null);
        }
        return this;
    }

    @Override
    public CriteriaQuery<T> groupBy(List<Expression<?>> grouping) {
        if (!grouping.isEmpty()) {
            query.setGroupBy(grouping);
        } else {
            query.setGroupBy(null);
        }
        return this;
    }

    @Override
    public CriteriaQuery<T> having(Expression<Boolean> restriction) {
        query.setHaving(cb.and(cb.wrapExpressionToPredicateWithRepair(restriction)));
        return this;
    }

    @Override
    public CriteriaQuery<T> having(Predicate... restrictions) {
        query.setHaving(cb.and(restrictions));
        return this;
    }

    /**
     * Method translates criteria query to SOQL query and returns its string representation.
     *
     * @param parameterFiller Generator of parameter values in the query string
     * @return string representation of SOQL query
     */
    public String translateQuery(CriteriaParameterFiller parameterFiller) {
        StringBuilder soqlQuery = new StringBuilder();
        soqlQuery.append(queryType.getKeyword());
        if (isDistinct()) {
            soqlQuery.append(' ').append(SoqlConstants.DISTINCT);
        }
        if (queryType == QueryType.SELECT) {
            soqlQuery.append(' ');
            ((AbstractExpression) query.getSelection()).setExpressionToQuery(soqlQuery, parameterFiller);
        }

        appendFromClause(parameterFiller, soqlQuery);

        appendWhereClause(parameterFiller, soqlQuery);

        appendGroupByClause(parameterFiller, soqlQuery);

        appendHavingClause(parameterFiller, soqlQuery);

        appendOrderByClause(parameterFiller, soqlQuery);

        return soqlQuery.toString();
    }

    private void appendFromClause(CriteriaParameterFiller parameterFiller, StringBuilder soqlQuery) {
        soqlQuery.append(' ').append(SoqlConstants.FROM).append(' ')
                 .append(((RootImpl) query.getRoot()).getJavaType().getSimpleName()).append(' ');
        ((RootImpl) query.getRoot()).setExpressionToQuery(soqlQuery, parameterFiller);
    }

    private void appendWhereClause(CriteriaParameterFiller parameterFiller, StringBuilder soqlQuery) {
        if (query.getWhere() != null && !query.getWhere().getExpressions().isEmpty()) {
            soqlQuery.append(' ').append(SoqlConstants.WHERE).append(' ');
            ((AbstractPredicate) query.getWhere()).setExpressionToQuery(soqlQuery, parameterFiller);
        }
    }

    private void appendGroupByClause(CriteriaParameterFiller parameterFiller, StringBuilder soqlQuery) {
        if (query.getGroupBy() != null && !query.getGroupBy().isEmpty()) {
            soqlQuery.append(' ').append(SoqlConstants.GROUP_BY).append(' ');
            for (Expression groupBy : query.getGroupBy()) {
                ((AbstractExpression) groupBy).setExpressionToQuery(soqlQuery, parameterFiller);
            }
        }
    }

    private void appendHavingClause(CriteriaParameterFiller parameterFiller, StringBuilder soqlQuery) {
        if (query.getHaving() != null && !query.getHaving().getExpressions().isEmpty()) {
            soqlQuery.append(" HAVING ");
            ((AbstractPredicate) query.getHaving()).setExpressionToQuery(soqlQuery, parameterFiller);
        }
    }

    private void appendOrderByClause(CriteriaParameterFiller parameterFiller, StringBuilder soqlQuery) {
        if (!getOrderList().isEmpty()) {
            soqlQuery.append(' ').append(SoqlConstants.ORDER_BY).append(' ');
            List<Order> orders = getOrderList();
            for (int i = 0; i < orders.size(); i++) {
                ((AbstractExpression) orders.get(i).getExpression()).setExpressionToQuery(soqlQuery, parameterFiller);
                soqlQuery.append(' ').append(orders.get(i).isAscending() ? SoqlConstants.ASC : SoqlConstants.DESC);
                if (orders.size() > 1 && (i + 1) != orders.size()) {
                    soqlQuery.append(", ");
                }
            }
        }
    }
}
