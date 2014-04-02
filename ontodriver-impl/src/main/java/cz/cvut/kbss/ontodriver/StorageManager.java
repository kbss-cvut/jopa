package cz.cvut.kbss.ontodriver;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Objects;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.exceptions.MetamodelNotSetException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * Manages the whole underlying ontology storage which can consist of several
 * ontologies or modules (OWL modules or RDF named graphs). </p>
 * 
 * The responsibility of a {@code StorageManager} is to provide unified access
 * to both OWL and RDF storages and thus represent a generic facade to multiple
 * types of ontology storages.
 * 
 * @author kidney
 * 
 */
public abstract class StorageManager implements Transactional {

	protected final PersistenceProviderFacade persistenceProvider;
	protected boolean open;

	public StorageManager(PersistenceProviderFacade persistenceProvider) {
		this.persistenceProvider = Objects.requireNonNull(persistenceProvider,
				ErrorUtils.constructNPXMessage("persistenceProvider"));
		this.open = true;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	/**
	 * {@inheritDoc} </p>
	 * 
	 * Implementing subclasses can (and should) override this method to close
	 * any open storage connections they maintain.
	 */
	@Override
	public void close() throws OntoDriverException {
		this.open = false;
	}

	/**
	 * Executes the specified SPARQL statement. </p>
	 * 
	 * If the statement is an update, then the number of affected individuals is
	 * returned in the result set.
	 * 
	 * @param statement
	 *            The statement to execute
	 * @return results of the execution
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code statement} is {@code null}
	 */
	public abstract ResultSet executeStatement(JopaStatement statement) throws OntoDriverException;

	/**
	 * Resolves whether the specified repository contains entity with the
	 * specified primary key. </p>
	 * 
	 * This method also searches imports declared by the specified repository.
	 * 
	 * @param primaryKey
	 *            Primary key
	 * @param repository
	 *            Repository to search in
	 * @return {@code true} if the {@code repository} contains entity with
	 *         {@code primaryKey}, {@code false} otherwise
	 * @throws OntoDriverException
	 *             If {@code repository} is not valid or if an ontology access
	 *             error occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey} or {@code repository} is {@code null}
	 */
	public abstract boolean contains(Object primaryKey, RepositoryID repository)
			throws OntoDriverException;

	/**
	 * Checks whether the specified ontology repository is consistent.
	 * 
	 * @param repository
	 *            The repository to check
	 * @return {@code true} if the repository is consistent, {@code false}
	 *         otherwise
	 * @throws OntoDriverException
	 *             If {@code entityContext} is not valid or if an ontology
	 *             access error occurs
	 * @throws NullPointerException
	 *             If {@code repository} is {@code null}
	 */
	public abstract boolean isConsistent(RepositoryID repository) throws OntoDriverException;

	/**
	 * Finds entity with the specified primary key and returns it as the
	 * specified entity type. </p>
	 * 
	 * The repository identifier may specify multiple contexts which will be
	 * searched.
	 * 
	 * @param cls
	 *            Return type
	 * @param primaryKey
	 *            Primary key
	 * @param repository
	 *            Repository identifier
	 * @return The found entity or {@code null}
	 * @throws OntoDriverException
	 *             If repository is not valid, if the {@code cls} is not an
	 *             entity class or if an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code cls}, or {@code primaryKey} or {@code repository}
	 *             is {@code null}
	 */
	public abstract <T> T find(Class<T> cls, Object primaryKey, RepositoryID repository)
			throws OntoDriverException;

	/**
	 * Returns a list of all available repository this {@code StorageManager} is
	 * managing. </p>
	 * 
	 * The returned list can never be empty.
	 * 
	 * @return List of contexts
	 */
	public abstract List<Repository> getRepositories();

	/**
	 * Loads from ontology and sets value of field {@code fieldName}. </p>
	 * 
	 * This method is intended to be used for lazy loaded entity fields.
	 * 
	 * @param entity
	 *            The entity to set field value on
	 * @param field
	 *            The field to load
	 * @param repository
	 *            Identifier of repository from which the field value should be
	 *            loaded
	 * @throws OntoDriverException
	 *             If called on a closed storage manager, if the repository is
	 *             not valid or if an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code entity}, {@code fieldName} or {@code repository} is
	 *             {@code null}
	 */
	public abstract <T> void loadFieldValue(T entity, Field field, RepositoryID repository)
			throws OntoDriverException;

	/**
	 * Merges the state of the specified entity field into the appropriate
	 * ontology. </p>
	 * 
	 * If the specified repository does not contain corresponding individual an
	 * exception is thrown. </p>
	 * 
	 * The repository identifier should specify exactly one context. If there
	 * are multiple the storage module will use the first returned by the
	 * collection's iterator.
	 * 
	 * @param primaryKey
	 *            Primary key of the merged entity
	 * @param entity
	 *            The merged entity
	 * @param mergedField
	 *            The field to merge
	 * @param repository
	 *            Repository identifier
	 * @throws OntoDriverException
	 *             If the entity is not persistent yet, if repository is not
	 *             valid or if an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey}, {@code entity} or {@code repository}
	 *             is {@code null}
	 */
	public abstract <T> void merge(Object primaryKey, T entity, Field mergedField,
			RepositoryID repository) throws OntoDriverException;

	/**
	 * Persists the specified entity. </p>
	 * 
	 * The {@code entity} is persisted along with its attributes into context
	 * specified by the {@code repository} parameter. </p>
	 * 
	 * @param primaryKey
	 *            Primary key of the persisted entity. Optional, if not set it
	 *            will be generated
	 * @param entity
	 *            The entity to persist
	 * @param repository
	 *            Target repository identifier
	 * @throws OntoDriverException
	 *             If the primary key is not set, if an entity with the
	 *             specified primary key already exists in the specified
	 *             context, if repository is not valid or if an ontology access
	 *             error occurs
	 * @throws NullPointerException
	 *             If {@code entity} or {@code repository} is {@code null}
	 */
	public abstract <T> void persist(Object primaryKey, T entity, RepositoryID repository)
			throws OntoDriverException;

	/**
	 * Removes entity with the specified primary key from the specified
	 * repository. </p>
	 * 
	 * If the repository does not contain any individual with the specified
	 * primary key an exception is thrown.
	 * 
	 * @param primaryKey
	 *            Primary key of the entity to remove
	 * @param repository
	 *            Repository from which the entity should be removed
	 * @throws OntoDriverException
	 *             If {@code entityContext} is not valid, if
	 *             {@code entityContext} does not contain any individual with
	 *             the specified primary key or if an ontology access error
	 *             occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey} or {@code entityContext} is null
	 */
	public abstract void remove(Object primaryKey, RepositoryID repository)
			throws OntoDriverException;

	/**
	 * Ensures that this storage manager is in valid status.
	 * 
	 * @throws IllegalStateException
	 * @throws MetamodelNotSetException
	 */
	protected void ensureState() throws OntoDriverException, MetamodelNotSetException {
		if (!open) {
			throw new IllegalStateException("The StorageManager is closed.");
		}
		if (persistenceProvider == null || persistenceProvider.getMetamodel() == null) {
			throw new MetamodelNotSetException();
		}
	}
}
