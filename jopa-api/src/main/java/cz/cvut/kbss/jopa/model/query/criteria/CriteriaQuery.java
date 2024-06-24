/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.query.criteria;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;

import java.util.List;

/**
 * Interface used to control the execution of typed queries.
 *
 * @param <T> Query result type
 */
public interface CriteriaQuery<T> {

    /**
     * Create and add a query root corresponding to the given entity class.
     *
     * @param entityClass the entity class
     * @return query root corresponding to the given entity
     */
    <X> Root<X> from(Class<X> entityClass);

    /**
     * Create and add a query root corresponding to the given entity type.
     *
     * @param entity metamodel entity representing the entity of type X
     * @return query root corresponding to the given entity
     */
    <X> Root<X> from(EntityType<X> entity);

    /**
     * Specify the item that is to be returned in the query result. Replaces the previously specified selection(s), if any.
     *
     * @param selection - selection specifying the item that is to be returned in the query result
     * @return the modified query
     */
    CriteriaQuery<T> select(Selection<? extends T> selection);

    /**
     * Modify the query to restrict the query result according to the specified boolean expression. Replaces the previously added restriction(s), if any.
     *
     * @param expression - a simple or compound boolean expression
     * @return the modified query
     */
    CriteriaQuery<T> where(Expression<Boolean> expression);

    /**
     * Modify the query to restrict the query result according to the conjunction of the specified restriction predicates. Replaces the previously added restriction(s), if any. If no restrictions are specified, any previously added restrictions are simply removed.
     *
     * @param predicates - zero or more restriction predicates
     * @return the modified query
     */
    CriteriaQuery<T> where(Predicate... predicates);

    /**
     * Modify the query to restrict the query result according to the conjunction of the specified restriction predicates. Replaces the previously added restriction(s), if any. If no restrictions are specified, any previously added restrictions are simply removed.
     *
     * @param predicates - list of predicates
     * @return the modified query
     */
    CriteriaQuery<T> where(List<Predicate> predicates);

    Class<T> getResultType();

    /**
     * Specify whether duplicate query results will be eliminated. A true value will cause duplicates to be eliminated. A false value will cause duplicates to be retained. If distinct has not been specified, duplicate results must be retained.
     *
     * @param distinct - boolean value specifying whether duplicate results must be eliminated from the query result or whether they must be retained
     * @return the modified query
     */
    CriteriaQuery<T> distinct(boolean distinct);

    /**
     * Specify that duplicates query results will be eliminated.
     *
     * @return the modified query
     */
    CriteriaQuery<T> distinct();

    /**
     * Return whether duplicate query results must be eliminated or retained.
     *
     * @return boolean indicating whether duplicate query results must be eliminated
     */
    boolean isDistinct();

    /**
     * Return the selection of the query, or null if no selection has been set.
     *
     * @return selection item
     */
    Selection<T> getSelection();

    /**
     * Return the predicate that corresponds to the where clause restriction(s), or null if no restrictions have been specified.
     *
     * @return where clause predicate
     */
    Predicate getRestriction();

    /**
     * Specify the expressions that are used to form groups over the query results. Replaces the previous specified grouping expressions, if any. If no grouping expressions are specified, any previously added grouping expressions are simply removed.
     *
     * @param grouping zero or more grouping expressions
     * @return the modified query
     */
    CriteriaQuery<T> groupBy(Expression<?>... grouping);

    /**
     * Specify the expressions that are used to form groups over the query results. Replaces the previous specified grouping expressions, if any. If no grouping expressions are specified, any previously added grouping expressions are simply removed.
     *
     * @param grouping list of zero or more grouping expressions
     * @return the modified query
     */
    CriteriaQuery<T> groupBy(List<Expression<?>> grouping);

    /**
     * Specify the ordering expressions that are used to order the query results. Replaces the previous ordering expressions, if any. If no ordering expressions are specified, the previous ordering, if any, is simply removed, and results will be returned in no particular order. The left-to-right sequence of the ordering expressions determines the precedence, whereby the leftmost has highest precedence.
     *
     * @param o list of zero or more ordering expressions
     * @return the modified query
     */
    CriteriaQuery<T> orderBy(List<Order> o);

    /**
     * Specify the ordering expressions that are used to order the query results. Replaces the previous ordering expressions, if any. If no ordering expressions are specified, the previous ordering, if any, is simply removed, and results will be returned in no particular order. The left-to-right sequence of the ordering expressions determines the precedence, whereby the leftmost has highest precedence.
     *
     * @param o zero or more ordering expressions
     * @return the modified query
     */
    CriteriaQuery<T> orderBy(Order... o);


    /**
     * Return the ordering expressions in order of precedence. Returns empty list if no ordering expressions have been specified.
     *
     * @return the list of ordering expressions
     */
    List<Order> getOrderList();

    /**
     * Specify a restriction over the groups of the query. Replaces the previous having restriction(s), if any.
     *
     * @param restriction a simple or compound boolean expression
     * @return the modified query
     */
    CriteriaQuery<T> having(Expression<Boolean> restriction);

    /**
     * Specify restrictions over the groups of the query according the conjunction of the specified restriction predicates. Replaces the previously added having restriction(s), if any. If no restrictions are specified, any previously added restrictions are simply removed.
     *
     * @param restrictions zero or more restriction predicates
     * @return the modified query
     */
    CriteriaQuery<T> having(Predicate... restrictions);
}
