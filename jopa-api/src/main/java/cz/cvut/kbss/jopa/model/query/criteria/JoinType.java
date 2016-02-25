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

/**
 * Defines the three types of joins.
 * 
 * Right outer joins and right outer fetch joins are not required to be
 * supported in Java Persistence 2.0. Applications that use RIGHT join types
 * will not be portable.
 */
public enum JoinType {
	/**
	 * Inner join.
	 */
	INNER,
	/**
	 * Left outer join.
	 */
	LEFT,
	/**
	 * Right outer join.
	 */
	RIGHT
}
