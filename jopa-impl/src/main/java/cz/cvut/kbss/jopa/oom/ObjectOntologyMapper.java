package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.oom.exceptions.UnpersistedChangeException;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;

public interface ObjectOntologyMapper {

    /**
     * Checks whether the storage contains individual with the specified
     * identifier and of the specified type. </p>
     *
     * @param cls        Class representing the individual type
     * @param primaryKey Identifier
     * @param descriptor Descriptor, can specify context
     * @return {@code true} if the ontology contains such individual,
     * {@code false} otherwise
     */
    public <T> boolean containsEntity(Class<T> cls, URI primaryKey, Descriptor descriptor);

    /**
     * Loads and reconstructs the entity from the ontology. </p>
     *
     * @param loadingParameters Entity loading parameters
     * @return Reconstructed entity or {@code null} if there is none such
     */
    public <T> T loadEntity(LoadingParameters<T> loadingParameters);

    /**
     * Loads entity field value and sets it on the specified entity. </p>
     *
     * @param entity     The entity on which the field value will be set
     * @param field      The field to load
     * @param descriptor Descriptor possibly specifying the field context
     */
    public <T> void loadFieldValue(T entity, Field field, Descriptor descriptor);

    /**
     * Persists the specified entity into the underlying ontology. </p>
     *
     * @param primaryKey Primary key of the persisted entity, possibly {@code null}
     * @param entity     The entity to persist
     * @param descriptor Descriptor possibly specifying entity and attribute contexts
     */
    public <T> void persistEntity(URI primaryKey, T entity, Descriptor descriptor);

    /**
     * Removes entity with specified identifier from the ontology.
     *
     * @param primaryKey Entity identifier
     * @param cls        Entity class
     * @param descriptor Descriptor specifying entity attribute contexts
     */
    public <T> void removeEntity(URI primaryKey, Class<T> cls, Descriptor descriptor);

    /**
     * Checks that there are no pending changes in the mapper. </p>
     *
     * @throws UnpersistedChangeException
     */
    public void checkForUnpersistedChanges();

    /**
     * Sets value of property represented by the specified field to the field's
     * value.
     *
     * @param entity     Entity containing the field
     * @param field      The field to update
     * @param descriptor Optionally specifies context
     */
    public <T> void updateFieldValue(T entity, Field field, Descriptor descriptor);
}
