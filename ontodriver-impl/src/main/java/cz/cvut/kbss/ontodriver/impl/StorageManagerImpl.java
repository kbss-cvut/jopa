package cz.cvut.kbss.ontodriver.impl;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

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
	private List<StorageModule> modules;
	private Map<Integer, StorageModule> modulesWithChanges;

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
		this.modulesWithChanges = new HashMap<Integer, StorageModule>();
		initModules();
	}

	@Override
	public void close() throws OntoDriverException {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing the storage manager.");
		}
		for (StorageModule m : modules) {
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
		checkForIdentifierValidity(statement.getRepository());
		final StorageModule m = getModule(statement.getRepository());
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
		checkForIdentifierValidity(repository);

		final StorageModule m = getModule(repository);
		return m.contains(primaryKey, repository);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, RepositoryID repository)
			throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Retrieving entity with primary key " + primaryKey + " from repository "
					+ repository);
		}
		ensureState();
		Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));
		checkForIdentifierValidity(repository);

		final StorageModule module = getModule(repository);
		final T entity = module.find(cls, primaryKey, repository);
		return entity;
	}

	@Override
	public boolean isConsistent(RepositoryID repository) throws OntoDriverException {
		ensureState();
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));
		checkForIdentifierValidity(repository);

		final StorageModule m = getModule(repository);
		return m.isConsistent(repository);
	}

	@Override
	public List<Repository> getRepositories() {
		return Collections.unmodifiableList(repositories);
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, RepositoryID repository)
			throws OntoDriverException {
		ensureState();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(field, ErrorUtils.constructNPXMessage("field"));
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));
		checkForIdentifierValidity(repository);

		final StorageModule m = getModule(repository);
		m.loadFieldValue(entity, field, repository);
	}

	@Override
	public <T> void merge(Object primaryKey, T entity, Field mergedField, RepositoryID repository)
			throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Merging entity with primary key " + primaryKey + " into repository "
					+ repository);
		}
		ensureState();
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(mergedField, ErrorUtils.constructNPXMessage("mergedField"));
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));
		checkForIdentifierValidity(repository);

		final StorageModule module = getModule(repository);
		module.merge(primaryKey, entity, mergedField, repository);
		modulesWithChanges.put(repository.getRepository(), module);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity, RepositoryID repository)
			throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Persisting entity into repository " + repository);
		}
		ensureState();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));
		checkForIdentifierValidity(repository);

		final StorageModule module = getModule(repository);
		module.persist(primaryKey, entity, repository);
		modulesWithChanges.put(repository.getRepository(), module);
	}

	@Override
	public void remove(Object primaryKey, RepositoryID repository) throws OntoDriverException {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Removing entity with primary key " + primaryKey + " from repository "
					+ repository);
		}
		ensureState();
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));
		checkForIdentifierValidity(repository);

		StorageModule module = getModule(repository);
		module.remove(primaryKey, repository);
		modulesWithChanges.put(repository.getRepository(), module);
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
	}

	private void checkForIdentifierValidity(RepositoryID identifier) throws OntoDriverException {
		assert identifier != null;
		if (identifier.getRepository() < 0) {
			throw new IllegalArgumentException("Unknown repository identifier " + identifier
					+ ". No such repository is managed by this StorageManager.");
		}
	}

	private void initModules() {
		this.modules = new ArrayList<>(repositories.size());
		final int len = repositories.size();
		for (int i = 0; i < len; i++) {
			modules.add(null);
		}
	}

	private StorageModule getModule(RepositoryID identifier) throws OntoDriverException {
		assert identifier != null;
		assert (identifier.getRepository() >= 0 && identifier.getRepository() < repositories.size());

		StorageModule m = modules.get(identifier.getRepository());
		if (m == null) {
			final Repository r = repositories.get(identifier.getRepository());
			m = driver.getFactory(r).createStorageModule(identifier, persistenceProvider, false);
			modules.set(r.getId(), m);
		}
		return m;
	}
}
