package cz.cvut.kbss.owlpersistence.model.query.criteria;

import cz.cvut.kbss.owlpersistence.model.metamodel.Attribute;

/**
 * Represents a join-fetched association or attribute.
 * 
 * @param <Z>
 *            the source type of the fetch
 * @param <X>
 *            the target type of the fetch
 */
public interface Fetch<Z, X> extends FetchParent<Z, X> {
	/**
	 * Return the metamodel attribute corresponding to the fetch join.
	 * 
	 * @return metamodel attribute for the join
	 */
	Attribute<? super Z, ?> getAttribute();

	/**
	 * Return the parent of the fetched item.
	 * 
	 * @return fetch parent
	 */
	FetchParent<?, Z> getParent();

	/**
	 * Return the join type used in the fetch join.
	 * 
	 * @return join type
	 */
	JoinType getJoinType();
}
