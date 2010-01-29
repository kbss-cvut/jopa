package cz.cvut.kbss.owlpersistence.model;

public interface PersistenceUnitUtil {

	public boolean isLoaded(Object entity, String attributeName);

	public boolean isLoaded(Object entity);

	/**
	 * Return the id of the entity. A generated id is not guaranteed to be
	 * available until after the database insert has occurred. Returns null if
	 * the entity does not yet have an id.
	 * 
	 * @param entity
	 *            entity instance
	 * @return id of the entity
	 * @throws IllegalArgumentException
	 *             if the object is found not to be an entity
	 */
	public Object getIdentifier(Object entity);
}
