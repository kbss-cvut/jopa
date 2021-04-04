/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.criteria.*;
import cz.cvut.kbss.jopa.query.criteria.*;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractAggregateFunctionExpression;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractExpression;
import cz.cvut.kbss.jopa.query.criteria.expressions.AbstractPathExpression;
import cz.cvut.kbss.jopa.query.criteria.expressions.ExpressionEntityImpl;
import cz.cvut.kbss.jopa.sessions.CriteriaFactory;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

//TODO PRO - CriteriaQueryImpl methods implementation
public class CriteriaQueryImpl<T> implements CriteriaQuery<T> {

    private static final Logger LOG = LoggerFactory.getLogger(CriteriaQuery.class);

    protected final CriteriaQueryHolder<T> query;
    private final Metamodel metamodel;
    private final CriteriaFactoryImpl factory;


    public CriteriaQueryImpl(CriteriaQueryHolder<T> query, Metamodel metamodel, CriteriaFactoryImpl factory) {
        this.query = Objects.requireNonNull(query, ErrorUtils.getNPXMessageSupplier("query"));
        this.metamodel = metamodel;
        this.factory = factory;
    }

    @Override
    public <X> Root<X> from(Class<X> entityClass) {
        RootImpl<X> root = new RootImpl<>(metamodel, new ExpressionEntityImpl<>(entityClass, null, metamodel), entityClass);
        query.setRoot(root);
        return root;
    }

    @Override
    public <X> Root<X> from(EntityType<X> entity) {
        return null;
    }

    @Override
    public CriteriaQuery<T> select(Selection<? extends T> selection) {
        query.setSelection((SelectionImpl<? extends T>) selection);
        return this;
    }

    @Override
    public CriteriaQuery<T> where(Expression<Boolean> expression) {
        query.setWhere(factory.wrapExpressionToPredicateWithRepair(expression));
        return this;
    }

    @Override
    public CriteriaQuery<T> where(Predicate... predicates) {
        query.setWhere(factory.and(predicates));
        return this;
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
        if (!o.isEmpty()) query.setOrderBy(o);
        else query.setOrderBy(null);
        return this;
    }

    @Override
    public CriteriaQuery<T> orderBy(Order... o) {
        if (o != null && o.length > 0) query.setOrderBy(Arrays.asList(o));
        else query.setOrderBy(null);
        return this;
    }

    @Override
    public List<Order> getOrderList() {
        if (query.getOrderBy() == null) return Collections.emptyList();
        return query.getOrderBy();
    }

    //TODO - BAKALARKA - KONZULTACIA
    // je potrebne pred generovanim HAVING kontrolovat napr. ci obsahuje GROUP BY?
    // cital som ze SQL dotaz kde je HAVING ale nie je GROUP BY je validny
    // ale CriteriaAPI od Hibernate pri vynechani GROUP BY vynechava aj HAVING
    @Override
    public CriteriaQuery<T> groupBy(Expression<?>... grouping) {
        if (grouping != null && grouping.length > 0) query.setGroupBy(Arrays.asList(grouping));
        else query.setGroupBy(null);
        return this;
    }

    @Override
    public CriteriaQuery<T> groupBy(List<Expression<?>> grouping) {
        if (!grouping.isEmpty()) query.setGroupBy(grouping);
        else query.setGroupBy(null);
        return this;
    }

    //TODO - BAKALARKA - KONZULTACIA
    // problem s vyrazom, ktory nie je Booleanovsky ale tvary sa tak
    // Expression<Boolean> expBool = root.get("username");
    // riesenie?
    // pri kazdej metode, ktorá ma akceptovat Expression<Boolean> preverím
    // if (restriction instanceof AbstractPathExpression){
    //      new ExpressionEquals(restriction, null)
    // }
    // ??
    @Override
    public CriteriaQuery<T> having(Expression<Boolean> restriction) {
        factory.and(factory.wrapExpressionToPredicateWithRepair(restriction));
        return this;
    }

    @Override
    public CriteriaQuery<T> having(Predicate... restrictions) {
        factory.and(restrictions);
        return this;
    }

    public String translateQuery(CriteriaParameterFiller parameterFiller){
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("SELECT ");
        if (isDistinct()) stringBuilder.append("DISTINCT ");
        ((AbstractExpression)query.getSelection()).setExpressionToQuery(stringBuilder,parameterFiller);

        stringBuilder.append(" FROM "+ ((RootImpl)query.getRoot()).getJavaType().getSimpleName()+ " ");
        ((RootImpl)query.getRoot()).setExpressionToQuery(stringBuilder, parameterFiller);

        if (query.getWhere() != null){
            stringBuilder.append(" WHERE ");
            ((AbstractPredicate)query.getWhere()).setExpressionToQuery(stringBuilder, parameterFiller);
        }

        if (!getOrderList().isEmpty()){
            stringBuilder.append(" ORDER BY ");
            List<Order> orders = getOrderList();
            for (int i = 0; i < orders.size(); i++) {
                //TODO - BAKALARKA - KONZULTACIA
                // em.createQuery("SELECT s from Student s WHERE s.age >= :age ORDER BY s ASC") podciarkuje "s" ale nepadne to chybou,
                // aj CriteriaAPI dotaz query.asc(root) je bez problemov
                // je to validne?
                // --
                //  Root.setExpressionToQuery -> Student s
                //  Root.getParentPath().setExpressionToQuery -> s
                //  bol problem s tym, ze prvu variantu potrebujem iba v FROM
                //  prerobil som tak, ze stale
                //  Root.setExpressionToQuery -> s
                //  a ked tvorim FROM tak este pre tym robim Root.getJavaClass.getSingleName

//                AbstractExpression expression = ((AbstractExpression)orders.get(i).getExpression());
//                if (expression instanceof PathImpl){
//                    ((AbstractPathExpression)((PathImpl) expression).getParentPath()).setExpressionToQuery(stringBuilder,parameterFiller);
//                } else {
//                    ((AbstractExpression)orders.get(i).getExpression()).setExpressionToQuery(stringBuilder,parameterFiller);
//                }

                ((AbstractExpression)orders.get(i).getExpression()).setExpressionToQuery(stringBuilder,parameterFiller);
                stringBuilder.append(orders.get(i).isAscending() ? " ASC" : " DESC");
                if (orders.size() > 1 && (i+1) != orders.size()) stringBuilder.append(", ");
            }
        }
        return stringBuilder.toString();
    }
}
