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

package cz.cvut.kbss.jopa.model.query.criteria;

import java.util.List;

import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;

/**
 * The ListJoin interface is the type of the result of joining to a collection
 * over an association or element collection that has been specified as a
 * java.util.List.
 * 
 * @param <Z>
 *            the source type of the join
 * @param <E>
 *            the element type of the target List
 */
public interface ListJoin<Z, E> extends PluralJoin<Z, List<E>, E> {
	/**
	 * Return the metamodel representation for the list attribute.
	 * 
	 * @return metamodel type representing the List that is
	 * 
	 *         the target of the join
	 */
	ListAttribute<? super Z, E> getModel();

	/**
	 * Create an expression that corresponds to the index of the object in the
	 * referenced association or element collection. This method must only be
	 * invoked upon an object that represents an association or element
	 * collection for which an order column has been defined.
	 * 
	 * @return expression denoting the index
	 */
	Expression<Integer> index();
}
