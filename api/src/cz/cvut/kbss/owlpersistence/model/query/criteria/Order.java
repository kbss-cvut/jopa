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

/**
 * An object that defines an ordering over the query results.
 */
public interface Order {
	/**
	 * Switch the ordering.
	 * 
	 * @return a new Order instance with the reversed ordering
	 */
	Order reverse();

	/**
	 * Whether ascending ordering is in effect.
	 * 
	 * @return boolean indicating whether ordering is ascending
	 */
	boolean isAscending();

	/**
	 * Return the expression that is used for ordering.
	 * 
	 * @return expression used for ordering
	 */
	Expression<?> getExpression();
}
