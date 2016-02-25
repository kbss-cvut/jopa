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

import java.util.Collection;

/**
 * Type for query expressions.
 * 
 * @param <T>
 *            the type of the expression
 */
public interface Expression<T> extends Selection<T> {
	/**
	 * Create a predicate to test whether the expression is null.
	 * 
	 * @return predicate testing whether the expression is null
	 */
	Predicate isNull();

	/**
	 * Create a predicate to test whether the expression is not null.
	 * 
	 * @return predicate testing whether the expression is not null
	 */
	Predicate isNotNull();

	/**
	 * Create a predicate to test whether the expression is a member of the
	 * argument list.
	 * 
	 * @param values
	 *            values to be tested against
	 * @return predicate testing for membership
	 */
	Predicate in(Object... values);

	/**
	 * Create a predicate to test whether the expression is a member of the
	 * argument list.
	 * 
	 * @param values
	 *            expressions to be tested against
	 * @return predicate testing for membership
	 */
	Predicate in(Expression<?>... values);

	/**
	 * Create a predicate to test whether the expression is a member of the
	 * collection.
	 * 
	 * @param values
	 *            collection of values to be tested against
	 * @return predicate testing for membership
	 */
	Predicate in(Collection<?> values);

	/**
	 * Create a predicate to test whether the expression is a member of the
	 * collection.
	 * 
	 * @param values
	 *            expression corresponding to collection to be
	 * 
	 *            tested against
	 * @return predicate testing for membership
	 */
	Predicate in(Expression<Collection<?>> values);

	/**
	 * Perform a typecast upon the expression, returning a new expression
	 * object. This method does not cause type conversion: the runtime type is
	 * not changed. Warning: may result in a runtime failure.
	 * 
	 * @param type
	 *            intended type of the expression
	 * @return new expression of the given type
	 */
	<X> Expression<X> as(Class<X> type);
}
