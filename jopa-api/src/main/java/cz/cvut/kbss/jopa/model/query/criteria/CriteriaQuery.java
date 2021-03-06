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
package cz.cvut.kbss.jopa.model.query.criteria;

/**
 * Interface used to control the execution of typed queries.
 *
 * @param <T> Query result type
 */
public interface CriteriaQuery<T>{

    /**
     * Specify the item that is to be returned in the query result. Replaces the previously specified selection(s), if any.
     * @param selection - selection specifying the item that is to be returned in the query result
     * @return the modified query
     */
    CriteriaQuery<T> select(Selection<? extends T> selection);

    /**
     * Modify the query to restrict the query result according to the specified boolean expression. Replaces the previously added restriction(s), if any.
     * @param expression - a simple or compound boolean expression
     * @return the modified query
     */
    CriteriaQuery<T> where(Expression<Boolean> expression);

    /**
     * Modify the query to restrict the query result according to the conjunction of the specified restriction predicates. Replaces the previously added restriction(s), if any. If no restrictions are specified, any previously added restrictions are simply removed.
     * @param predicates - zero or more restriction predicates
     * @return
     */
    CriteriaQuery<T> where(Predicate... predicates);

    Class<T> getResultType();

    /**
     * Specify whether duplicate query results will be eliminated. A true value will cause duplicates to be eliminated. A false value will cause duplicates to be retained. If distinct has not been specified, duplicate results must be retained.
     * @param distinct - boolean value specifying whether duplicate results must be eliminated from the query result or whether they must be retained
     * @return the modified query
     */
    CriteriaQuery<T> distinct(boolean distinct);

    /**
     * Return whether duplicate query results must be eliminated or retained.
     * @return boolean indicating whether duplicate query results must be eliminated
     */
    boolean isDistinct();

    /**
     * Return the selection of the query, or null if no selection has been set.
     * @return selection item
     */
    Selection<T> getSelection();

    /**
     * Return the predicate that corresponds to the where clause restriction(s), or null if no restrictions have been specified.
     * @return where clause predicate
     */
    Predicate getRestriction();
}
