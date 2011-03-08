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
