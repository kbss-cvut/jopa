package cz.cvut.kbss.ontodriver.impl;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.StorageManager;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class StorageManagerImpl extends StorageManager {

	private static final Logger LOG = Logger.getLogger(StorageManagerImpl.class.getName());

	/** Reference to the driver */
	private final OntoDriverImpl driver;

	/** Repositories */
	private List<Repository> repositories;
	/** Storage modules */
	private final Map<EntityDescriptor, StorageModule> modules;
	private final Map<EntityDescriptor, StorageModule> modulesWithChanges;

	/**
	 * Constructor
	 * 
	 * @param metamodel
	 *            Metamodel
	 * @param repositories
	 *            List of available contexts
	 */
	public StorageManagerImpl(PersistenceProviderFacade persistenceProvider,
			List<Repository> repositories, OntoDriverImpl driver) {
		super(persistenceProvider);
		this.repositories = Objects.requireNonNull(repositories,
				ErrorUtils.constructNPXMessage("repositories"));
		if (repositories.isEmpty()) {
			throw new IllegalArgumentException("Contexts list cannot be empty.");
		}
		this.driver = Objects.requireNonNull(driver, ErrorUtils.constructNPXMessage("driver"));
		this.modulesWithChanges = new HashMap<>(repositories.size());
		this.modules = new HashMap<>(repositories.size());
	}

	@Override
	public void close() throws OntoDriverException {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing the storage manager.");
		}
		for (StorageModule m : modules.values()) {
			// Just close the module, any pending changes will be rolled back
			// implicitly
			if (m != null) {
				driver.getFactory(m.getRepositoryID()).releaseStorageModule(m);
			}
		}
		super.close();
	}

	@Override
	public void commit() throws OntoDriverException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Committing changes.");
		}
		ensureState();
		commitInternal();
	}

	@Override
	public ResultSet executeStatement(JopaStatement statement) throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Executing statement.");
		}
		ensureState();
		Objects.requireNonNull(statement, ErrorUtils.constructNPXMessage("statement"));
		Objects.requireNonNull(statement.getRepository(),
				ErrorUtils.constructNPXMessage("statement.getRepository"));

		final StorageModule m = getModule(statement.getRepository());
		return m.executeStatement(statement);
	}

	@Override
	public boolean contains(Object primaryKey, EntityDescriptor repository) throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Checking whether repository " + repository
					+ " contains entity with primary key " + primaryKey);
		}
		ensureState();
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));

		final StorageModule m = getModule(repository);
		return m.contains(primaryKey, repository);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, EntityDescriptor repository)
			throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Retrieving entity with primary key " + primaryKey + " from repository "
					+ repository);
		}
		ensureState();
		Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));

		final StorageModule module = getModule(repository);
		final T entity = module.find(cls, primaryKey, repository);
		return entity;
	}

	@Override
	public boolean isConsistent(EntityDescriptor repository) throws OntoDriverException {
		ensureState();
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));

		final StorageModule m = getModule(repository);
		return m.isConsistent(repository);
	}

	@Override
	public List<Repository> getRepositories() {
		return Collections.unmodifiableList(repositories);
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, EntityDescriptor repository)
			throws OntoDriverException {
		ensureState();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(field, ErrorUtils.constructNPXMessage("field"));
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));

		final StorageModule m = getModule(repository);
		m.loadFieldValue(entity, field, repository);
	}

	@Override
	public <T> void merge(T entity, Field mergedField, EntityDescriptor repository)
			throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Merging entity " + entity + ", field " + mergedField + " into repository "
					+ repository);
		}
		ensureState();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(mergedField, ErrorUtils.constructNPXMessage("mergedField"));
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));

		final StorageModule module = getModule(repository);
		module.merge(entity, mergedField, repository);
		moduleChanged(module);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity, EntityDescriptor repository)
			throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Persisting entity into repository " + repository);
		}
		ensureState();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));

		final StorageModule module = getModule(repository);
		module.persist(primaryKey, entity, repository);
		moduleChanged(module);
	}

	@Override
	public void remove(Object primaryKey, EntityDescriptor repository) throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Removing entity with primary key " + primaryKey + " from repository "
					+ repository);
		}
		ensureState();
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));

		StorageModule module = getModule(repository);
		module.remove(primaryKey, repository);
		moduleChanged(module);
	}

	@Override
	public void rollback() throws OntoDriverException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Rolling back changes.");
		}
		ensureState();
		rollbackInternal();
	}

	private void commitInternal() throws OntoDriverException {
		for (StorageModule module : modulesWithChanges.values()) {
			module.commit();
		}
		modulesWithChanges.clear();
	}

	private void rollbackInternal() throws OntoDriverException {
		for (StorageModule module : modulesWithChanges.values()) {
			module.rollback();
		}
		modulesWithChanges.clear();
	}

	private void moduleChanged(StorageModule m) {
		assert m != null;
		modulesWithChanges.put(m.getRepositoryID(), m);
	}

	private StorageModule getModule(EntityDescriptor identifier) throws OntoDriverException {
		assert identifier != null;

		StorageModule m = modules.get(identifier);
		if (m == null) {
			m = driver.getFactory(identifier).createStorageModule(identifier, persistenceProvider,
					false);
			modules.put(identifier, m);
		}
		return m;
	}
}
