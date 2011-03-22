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

import java.util.List;

/**
 * The type of a simple or compound predicate: a conjunction or disjunction of
 * restrictions. A simple predicate is considered to be a conjunction with a
 * single conjunct.
 */
public interface Predicate extends Expression<Boolean> {
	public static enum BooleanOperator {
		AND, OR
	}

	/**
	 * Return the boolean operator for the predicate. If the predicate is
	 * simple, this is AND.
	 * 
	 * @return boolean operator for the predicate
	 */
	BooleanOperator getOperator();

	/**
	 * Whether the predicate has been created from another predicate by applying
	 * the Predicate not() method or the CriteriaBuilder not() method.
	 * 
	 * @return boolean indicating if the predicate is
	 * 
	 *         a negated predicate
	 */
	boolean isNegated();

	/**
	 * Return the top-level conjuncts or disjuncts of the predicate. Returns
	 * empty list if there are no top-level conjuncts or disjuncts of the
	 * predicate. Modifications to the list do not affect the query.
	 * 
	 * @return list of boolean expressions forming the predicate
	 */
	List<Expression<Boolean>> getExpressions();

	/**
	 * Create a negation of the predicate.
	 * 
	 * @return negated predicate
	 */
	Predicate not();
}
