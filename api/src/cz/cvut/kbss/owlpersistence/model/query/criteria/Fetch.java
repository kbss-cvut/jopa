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

import cz.cvut.kbss.owlpersistence.model.metamodel.Attribute;

/**
 * Represents a join-fetched association or attribute.
 * 
 * @param <Z>
 *            the source type of the fetch
 * @param <X>
 *            the target type of the fetch
 */
public interface Fetch<Z, X> extends FetchParent<Z, X> {
	/**
	 * Return the metamodel attribute corresponding to the fetch join.
	 * 
	 * @return metamodel attribute for the join
	 */
	Attribute<? super Z, ?> getAttribute();

	/**
	 * Return the parent of the fetched item.
	 * 
	 * @return fetch parent
	 */
	FetchParent<?, Z> getParent();

	/**
	 * Return the join type used in the fetch join.
	 * 
	 * @return join type
	 */
	JoinType getJoinType();
}
