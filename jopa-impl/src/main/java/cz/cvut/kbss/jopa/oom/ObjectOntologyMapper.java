package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.oom.exceptions.UnpersistedChangeException;

public interface ObjectOntologyMapper {

	/**
	 * Loads and reconstructs the entity from the ontology. </p>
	 * 
	 * @param cls
	 *            Entity type
	 * @param primaryKey
	 *            Entity primary key, i. e. individual URI
	 * @param descriptor
	 *            Descriptor possibly specifying entity and attribute contexts
	 * @return Reconstructed entity or {@code null} if there is none such
	 */
	public <T> T loadEntity(Class<T> cls, URI primaryKey, Descriptor descriptor);

	/**
	 * Loads entity field value and sets it on the specified entity. </p>
	 * 
	 * @param primaryKey
	 *            Entity primary key, i. e. individual URI
	 * @param entity
	 *            The entity on which the field value will be set
	 * @param field
	 *            The field to load
	 * @param descriptor
	 *            Descriptor possibly specifying the field context
	 */
	public <T> void loadFieldValue(T entity, Field field, Descriptor descriptor);

	/**
	 * Persists the specified entity into the underlying ontology. </p>
	 * 
	 * @param primaryKey
	 *            Primary key of the persisted entity, possibly {@code null}
	 * @param entity
	 *            The entity to persist
	 * @param descriptor
	 *            Descriptor possibly specifying entity and attribute contexts
	 */
	public <T> void persistEntity(URI primaryKey, T entity, Descriptor descriptor);
	
	/**
	 * Checks that there are no pending changes in the mapper. </p>
	 * 
	 * @throws UnpersistedChangeException
	 */
	public void checkForUnpersistedChanges();
}
