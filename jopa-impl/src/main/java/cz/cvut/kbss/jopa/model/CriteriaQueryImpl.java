/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.criteria.*;
import cz.cvut.kbss.jopa.query.criteria.*;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractExpression;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

import java.util.*;


public class CriteriaQueryImpl<T> implements CriteriaQuery<T> {

    protected final CriteriaQueryHolder<T> query;
    private final Metamodel metamodel;
    private final CriteriaBuilderImpl cb;


    public CriteriaQueryImpl(CriteriaQueryHolder<T> query, Metamodel metamodel, CriteriaBuilderImpl cb) {
        this.query = Objects.requireNonNull(query, ErrorUtils.getNPXMessageSupplier("query"));
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
        RootImpl<X> root = new RootImpl<>(metamodel, null, entity.getBindableJavaType(), this.cb);
        query.setRoot(root);
        return root;
    }

    @Override
    public CriteriaQuery<T> select(Selection<? extends T> selection) {
        query.setSelection(selection);
        return this;
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
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("SELECT ");
        if (isDistinct()) {
            stringBuilder.append("DISTINCT ");
        }
        ((AbstractExpression) query.getSelection()).setExpressionToQuery(stringBuilder, parameterFiller);

        stringBuilder.append(" FROM " + ((RootImpl) query.getRoot()).getJavaType().getSimpleName() + " ");
        ((RootImpl) query.getRoot()).setExpressionToQuery(stringBuilder, parameterFiller);

        if (query.getWhere() != null) {
            stringBuilder.append(" WHERE ");
            ((AbstractPredicate) query.getWhere()).setExpressionToQuery(stringBuilder, parameterFiller);
        }

        if (query.getGroupBy() != null && !query.getGroupBy().isEmpty()) {
            stringBuilder.append(" GROUP BY ");
            for (Expression groupBy : query.getGroupBy()) {
                ((AbstractExpression) groupBy).setExpressionToQuery(stringBuilder, parameterFiller);
            }
        }

        if (query.getHaving() != null) {
            stringBuilder.append(" HAVING ");
            ((AbstractPredicate) query.getHaving()).setExpressionToQuery(stringBuilder, parameterFiller);
        }

        if (!getOrderList().isEmpty()) {
            stringBuilder.append(" ORDER BY ");
            List<Order> orders = getOrderList();
            for (int i = 0; i < orders.size(); i++) {
                ((AbstractExpression) orders.get(i).getExpression()).setExpressionToQuery(stringBuilder, parameterFiller);
                stringBuilder.append(orders.get(i).isAscending() ? " ASC" : " DESC");
                if (orders.size() > 1 && (i + 1) != orders.size()) {
                    stringBuilder.append(", ");
                }
            }
        }
        return stringBuilder.toString();
    }
}
