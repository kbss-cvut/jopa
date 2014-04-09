package cz.cvut.kbss.ontodriver.impl;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
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
	private final Map<Repository, StorageModule> modules;
	private final Map<Repository, StorageModule> modulesWithChanges;

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
				driver.getFactory(m.getRepository()).releaseStorageModule(m);
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
		Objects.requireNonNull(statement.getRepositoryId(),
				ErrorUtils.constructNPXMessage("statement.getRepository"));

		final StorageModule m = getModule(statement.getRepositoryId().getRepository());
		return m.executeStatement(statement);
	}

	@Override
	public boolean contains(Object primaryKey, RepositoryID repository) throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Checking whether repository " + repository
					+ " contains entity with primary key " + primaryKey);
		}
		ensureState();
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));

		final StorageModule m = getModule(repository.getRepository());
		return m.contains(primaryKey, repository);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, EntityDescriptor descriptor)
			throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Retrieving entity with primary key " + primaryKey + " from repository "
					+ descriptor);
		}
		ensureState();
		Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

		final StorageModule module = getModule(descriptor.getRepository());
		final T entity = module.find(cls, primaryKey, descriptor);
		return entity;
	}

	@Override
	public boolean isConsistent(RepositoryID repository) throws OntoDriverException {
		ensureState();
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));

		final StorageModule m = getModule(repository.getRepository());
		return m.isConsistent(repository);
	}

	@Override
	public List<Repository> getRepositories() {
		return Collections.unmodifiableList(repositories);
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, EntityDescriptor descriptor)
			throws OntoDriverException {
		ensureState();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(field, ErrorUtils.constructNPXMessage("field"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

		final StorageModule m = getModule(descriptor.getRepository());
		m.loadFieldValue(entity, field, descriptor);
	}

	@Override
	public <T> void merge(T entity, Field mergedField, EntityDescriptor descriptor)
			throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Merging entity " + entity + ", field " + mergedField + " into repository "
					+ descriptor);
		}
		ensureState();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(mergedField, ErrorUtils.constructNPXMessage("mergedField"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

		final StorageModule module = getModule(descriptor.getRepository());
		module.merge(entity, mergedField, descriptor);
		moduleChanged(module);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity, EntityDescriptor descriptor)
			throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Persisting entity into repository " + descriptor.getRepositoryUri());
		}
		ensureState();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

		final StorageModule module = getModule(descriptor.getRepository());
		module.persist(primaryKey, entity, descriptor);
		moduleChanged(module);
	}

	@Override
	public void remove(Object primaryKey, EntityDescriptor descriptor) throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Removing entity with primary key " + primaryKey + " from repository "
					+ descriptor.getRepositoryUri());
		}
		ensureState();
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

		StorageModule module = getModule(descriptor.getRepository());
		module.remove(primaryKey, descriptor);
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
		modulesWithChanges.put(m.getRepository(), m);
	}

	private StorageModule getModule(Repository identifier) throws OntoDriverException {
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
