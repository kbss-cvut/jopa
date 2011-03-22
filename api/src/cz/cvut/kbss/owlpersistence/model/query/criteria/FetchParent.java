/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

package cz.cvut.kbss.owlpersistence.model.query.criteria;

import cz.cvut.kbss.owlpersistence.model.metamodel.PluralAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.SingularAttribute;

/**
 * Represents an element of the from clause which may function as the parent of
 * Fetches.
 * 
 * @param <Z>
 *            the source type
 * @param <X>
 *            the target type
 */
public interface FetchParent<Z, X> {
	/**
	 * Return the fetch joins that have been made from this type. Returns empty
	 * set if no fetch joins have been made from this type. Modifications to the
	 * set do not affect the query.
	 * 
	 * @return fetch joins made from this type
	 */
	java.util.Set<Fetch<X, ?>> getFetches();

	/**
	 * Create a fetch join to the specified single-valued attribute using an
	 * inner join.
	 * 
	 * @param attribute
	 *            target of the join
	 * @return the resulting fetch join
	 **/
	<Y> Fetch<X, Y> fetch(SingularAttribute<? super X, Y> attribute);

	/**
	 * Create a fetch join to the specified single-valued attribute using the
	 * given join type.
	 * 
	 * @param attribute
	 *            target of the join
	 * @param jt
	 *            join type
	 * @return the resulting fetch join
	 **/
	<Y> Fetch<X, Y> fetch(SingularAttribute<? super X, Y> attribute, JoinType jt);

	/**
	 * Create a fetch join to the specified collection-valued attribute using an
	 * inner join.
	 * 
	 * @param attribute
	 *            target of the join
	 * @return the resulting join
	 **/
	<Y> Fetch<X, Y> fetch(PluralAttribute<? super X, ?, Y> attribute);

	/**
	 * Create a fetch join to the specified collection-valued attribute using
	 * the given join type.
	 * 
	 * @param attribute
	 *            target of the join
	 * @param jt
	 *            join type
	 * @return the resulting join
	 **/
	<Y> Fetch<X, Y> fetch(PluralAttribute<? super X, ?, Y> attribute,
			JoinType jt);

	// String-based:
	/**
	 * Create a fetch join to the specified attribute using an inner join.
	 * 
	 * @param attributeName
	 *            name of the attribute for the target of the join
	 * @return the resulting fetch join
	 * @throws IllegalArgumentException
	 *             if attribute of the given name does not exist
	 **/
	<Y> Fetch<X, Y> fetch(String attributeName);

	/**
	 * Create a fetch join to the specified attribute using the given join type.
	 * 
	 * @param attributeName
	 *            name of the attribute for the target of the join
	 * @param jt
	 *            join type
	 * @return the resulting fetch join
	 * @throws IllegalArgumentException
	 *             if attribute of the given name does not exist
	 **/
	<Y> Fetch<X, Y> fetch(String attributeName, JoinType jt);
}