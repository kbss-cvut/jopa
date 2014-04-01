package cz.cvut.kbss.ontodriver.impl;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.StorageManager;
import cz.cvut.kbss.ontodriver.exceptions.MetamodelNotSetException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class ConnectionImpl implements Connection {

	private static final Logger LOG = Logger.getLogger(ConnectionImpl.class.getName());

	private final StorageManager storageManager;
	private final Map<Integer, Repository> repositories;

	private boolean open;
	private boolean hasChanges;
	private boolean autoCommit;

	public ConnectionImpl(StorageManager storageManager) throws OntoDriverException {
		this.storageManager = Objects.requireNonNull(storageManager,
				"Argument 'storageManager' cannot be null.");

		this.repositories = new HashMap<>();
		for (Repository r : storageManager.getRepositories()) {
			repositories.put(r.getId(), r);
		}
		this.open = true;
		this.hasChanges = false;
		this.autoCommit = true;
	}

	@Override
	public void close() throws OntoDriverException {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing the connection.");
		}
		if (!open) {
			return;
		}
		storageManager.close();
		this.open = false;
	}

	@Override
	public void commit() throws OntoDriverException, MetamodelNotSetException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Committing changes.");
		}
		ensureOpen(true);
		if (!hasChanges) {
			return;
		}
		storageManager.commit();
		afterTransactionFinished();
	}

	public Statement createStatement() throws OntoDriverException {
		return new JopaStatement(storageManager);
	}

	@Override
	public boolean contains(Object primaryKey, RepositoryID repository) throws OntoDriverException {
		ensureOpen(false);
		if (primaryKey == null) {
			LOG.severe("Null argument passed: primaryKey = " + primaryKey);
			throw new NullPointerException();
		}
		return storageManager.contains(primaryKey, repository);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, RepositoryID repository)
			throws OntoDriverException {
		ensureOpen(true);
		Objects.requireNonNull(cls, "Argument 'cls' cannot be null.");
		Objects.requireNonNull(primaryKey, "Argument 'primaryKey' cannot be null.");
		Objects.requireNonNull(repository, "Argument 'repository' cannot be null.");

		final T result = storageManager.find(cls, primaryKey, repository);
		return result;
	}

	@Override
	public boolean getAutoCommit() throws OntoDriverException {
		ensureOpen(false);
		return autoCommit;
	}

	@Override
	public boolean isConsistent(RepositoryID repository) throws OntoDriverException {
		ensureOpen(false);
		Objects.requireNonNull(repository, "Argument 'repository' cannot be null.");

		return storageManager.isConsistent(repository);
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, RepositoryID repository)
			throws OntoDriverException {
		ensureOpen(true);
		Objects.requireNonNull(entity, "Argument 'entity' cannot be null.");
		Objects.requireNonNull(field, "Argument 'field' cannot be null.");
		Objects.requireNonNull(repository, "Argument 'repository' cannot be null.");

		storageManager.loadFieldValue(entity, field, repository);
	}

	@Override
	public <T> void merge(Object primaryKey, T entity, Field mergedField, RepositoryID repository)
			throws OntoDriverException {
		ensureOpen(true);
		Objects.requireNonNull(primaryKey, "Argument 'primaryKey' cannot be null.");
		Objects.requireNonNull(entity, "Argument 'entity' cannot be null.");
		Objects.requireNonNull(mergedField, "Argument 'mergedField' cannot be null.");
		Objects.requireNonNull(repository, "Argument 'repository' cannot be null.");

		storageManager.merge(primaryKey, entity, mergedField, repository);
		this.hasChanges = true;
		if (autoCommit) {
			commit();
		}
	}

	@Override
	public <T> void persist(Object primaryKey, T entity, RepositoryID repository)
			throws OntoDriverException {
		ensureOpen(true);
		// Primary key can be null
		Objects.requireNonNull(entity, "Argument 'entity' cannot be null.");
		Objects.requireNonNull(repository, "Argument 'repository' cannot be null.");

		storageManager.persist(primaryKey, entity, repository);
		this.hasChanges = true;
		if (autoCommit) {
			commit();
		}
	}

	@Override
	public PreparedStatement prepareStatement(String sparql) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> void remove(Object primaryKey, RepositoryID repository) throws OntoDriverException {
		ensureOpen(true);
		Objects.requireNonNull(primaryKey, "Argument 'primaryKey' cannot be null.");
		Objects.requireNonNull(repository, "Argument 'repository' cannot be null.");

		storageManager.remove(primaryKey, repository);
		this.hasChanges = true;
		if (autoCommit) {
			commit();
		}
	}

	@Override
	public void rollback() throws OntoDriverException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Rolling back changes.");
		}
		ensureOpen(false);
		if (!hasChanges) {
			return;
		}
		storageManager.rollback();
		afterTransactionFinished();
	}

	@Override
	public void setAutoCommit(boolean autoCommit) throws OntoDriverException {
		ensureOpen(false);
		this.autoCommit = autoCommit;
	}

	@Override
	public Repository getRepository(Integer repositoryId) throws OntoDriverException {
		ensureOpen(false);
		Objects.requireNonNull(repositoryId, "Argument 'repositoryId' cannot be null.");

		return repositories.get(repositoryId);
	}

	@Override
	public List<Repository> getRepositories() throws OntoDriverException {
		return storageManager.getRepositories();
	}

	/**
	 * Does cleanup after transaction has finished (either with {@code commit}
	 * or {@code rollback});
	 */
	private void afterTransactionFinished() {
		this.hasChanges = false;
	}

	/**
	 * Ensures correct state of this {@code Connection}. </p>
	 * 
	 * This means checking if it is open and, if enabled, whether the metamodel
	 * is set.
	 * 
	 * @param checkMetamodel
	 *            True if the metamodel should be checked
	 * @throws OntoDriverException
	 * @throws MetamodelNotSetException
	 */
	private void ensureOpen(boolean checkMetamodel) throws OntoDriverException,
			MetamodelNotSetException {
		if (!open) {
			throw new OntoDriverException("The connection is closed.");
		}
	}
}
