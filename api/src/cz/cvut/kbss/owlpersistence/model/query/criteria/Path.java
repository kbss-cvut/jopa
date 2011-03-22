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

import cz.cvut.kbss.owlpersistence.model.metamodel.Bindable;
import cz.cvut.kbss.owlpersistence.model.metamodel.MapAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.PluralAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.SingularAttribute;

/**
 * Represents a simple or compound attribute path from a bound type or
 * collection, and is a "primitive" expression.
 * 
 * @param <X>
 *            the type referenced by the path
 */
public interface Path<X> extends Expression<X> {
	/**
	 * Return the bindable object that corresponds to the path expression.
	 * 
	 * @return bindable object corresponding to the path
	 */
	Bindable<X> getModel();

	/**
	 * Return the parent "node" in the path or null if no parent.
	 * 
	 * @return parent
	 */
	Path<?> getParentPath();

	/**
	 * Create a path corresponding to the referenced single-valued attribute.
	 * 
	 * @param attribute
	 *            single-valued attribute
	 * @return path corresponding to the referenced attribute
	 **/
	<Y> Path<Y> get(SingularAttribute<? super X, Y> attribute);

	/**
	 * Create a path corresponding to the referenced collection-valued
	 * attribute.
	 * 
	 * @param collection
	 *            collection-valued attribute
	 * @return expression corresponding to the referenced attribute
	 **/
	<E, C extends java.util.Collection<E>> Expression<C> get(
			PluralAttribute<X, C, E> collection);

	/**
	 * Create a path corresponding to the referenced map-valued attribute.
	 * 
	 * @param map
	 *            map-valued attribute
	 * @return expression corresponding to the referenced attribute
	 **/
	<K, V, M extends java.util.Map<K, V>> Expression<M> get(
			MapAttribute<X, K, V> map);

	/**
	 * Create an expression corresponding to the type of the path.
	 * 
	 * @return expression corresponding to the type of the path
	 */
	Expression<Class<? extends X>> type();

	// String-based:
	/**
	 * Create a path corresponding to the referenced attribute. Note:
	 * Applications using the string-based API may need to specify the type
	 * resulting from the get operation in order to avoid the use of Path
	 * variables. For example: CriteriaQuery<Person> q =
	 * cb.createQuery(Person.class); Root<Person> p = q.from(Person.class);
	 * q.select(p) .where(cb.isMember("joe", p.<Set<String>>get("nicknames")));
	 * rather than: CriteriaQuery<Person> q = cb.createQuery(Person.class);
	 * Root<Person> p = q.from(Person.class); Path<Set<String>> nicknames =
	 * p.get("nicknames"); q.select(p) .where(cb.isMember("joe", nicknames));
	 * 
	 * @param attributeName
	 *            name of the attribute
	 * @return path corresponding to the referenced attribute
	 * @throws IllegalStateException
	 *             if invoked on a path that corresponds to a basic type
	 * @throws IllegalArgumentException
	 *             if attribute of the given name does not otherwise exist
	 **/
	<Y> Path<Y> get(String attributeName);
}