package cz.cvut.kbss.owlpersistence.model.query.criteria;

import java.util.Set;

import cz.cvut.kbss.owlpersistence.model.metamodel.SetAttribute;

/**
 * The CollectionJoin interface is the type of the result of joining to a
 * collection over an association or element collection that has been specified
 * as a java.util.Collection.
 * 
 * @param <Z>
 *            the source type of the join
 * @param <E>
 *            the element type of the target Collection
 */
public interface SetJoin<Z, E> extends PluralJoin<Z, Set<E>, E> {
	/**
	 * Return the metamodel representation for the collection attribute.
	 * 
	 * @return metamodel type representing the Collection that is
	 * 
	 *         the target of the join
	 */
	SetAttribute<? super Z, E> getModel();
}
