/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import java.util.Map;

import cz.cvut.kbss.jopa.model.metamodel.MapAttribute;

/**
 * The MapJoin interface is the type of the result of joining to a collection
 * over an association or element collection that has been specified as a
 * java.util.Map.
 * 
 * @param <Z>
 *            the source type of the join
 * @param <K>
 *            the type of the target Map key
 * @param <V>
 *            the type of the target Map value
 */
public interface MapJoin<Z, K, V> extends PluralJoin<Z, Map<K, V>, V> {
	/**
	 * Return the metamodel representation for the map attribute.
	 * 
	 * @return metamodel type representing the Map that is
	 * 
	 *         the target of the join
	 */
	MapAttribute<? super Z, K, V> getModel();

	/**
	 * Create a path expression that corresponds to the map key.
	 * 
	 * @return path corresponding to map key
	 */
	Path<K> key();

	/**
	 * Create a path expression that corresponds to the map value. This method
	 * is for stylistic use only: it just returns this.
	 * 
	 * @return path corresponding to the map value
	 */
	Path<V> value();

	/**
	 * Create an expression that corresponds to the map entry.
	 * 
	 * @return expression corresponding to the map entry
	 */
	Expression<Map.Entry<K, V>> entry();
}
