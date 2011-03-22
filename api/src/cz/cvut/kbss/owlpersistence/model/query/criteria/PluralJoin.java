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

/**
 * The PluralJoin interface defines functionality that is common to joins to all
 * collection types. It is not intended to be used directly in query
 * construction.
 * 
 * @param <Z>
 *            the source type
 * @param <C>
 *            the collection type
 * @param <E>
 *            the element type of the collection
 */
public interface PluralJoin<Z, C, E> extends Join<Z, E> {
	/**
	 * Return the metamodel representation for the collection-valued attribute
	 * corresponding to the join.
	 * 
	 * @return metamodel collection-valued attribute corresponding
	 * 
	 *         to the target of the join
	 */
	PluralAttribute<? super Z, C, E> getModel();
}
