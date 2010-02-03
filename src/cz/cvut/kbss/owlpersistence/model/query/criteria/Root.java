package cz.cvut.kbss.owlpersistence.model.query.criteria;

import cz.cvut.kbss.owlpersistence.model.metamodel.EntityType;

/**
 * A root type in the from clause. Query roots always reference entities.
 * 
 * @param <X>
 *            the entity type referenced by the root
 */
public interface Root<X> extends From<X, X> {
	/**
	 * Return the metamodel entity corresponding to the root.
	 * 
	 * @return metamodel entity corresponding to the root
	 */
	EntityType<X> getModel();
}
