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

package cz.cvut.kbss.jopa.model.query;

/**
 * The TupleElement interface defines an element that is returned in a query
 * result tuple.
 * 
 * @param <X>
 *            the type of the element
 */
public interface TupleElement<X> {
	/**
	 * Return the runtime Java type of the tuple element.
	 * 
	 * @return the runtime Java type of the tuple element
	 */
	Class<? extends X> getJavaType();

	/**
	 * Return the alias assigned to the tuple element or null, if no alias has
	 * been assigned.
	 * 
	 * @return alias
	 */
	String getAlias();
}
