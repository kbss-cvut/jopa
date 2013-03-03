package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.PersistenceProvider;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class OwlapiStorageModule extends StorageModule {

	private OwlapiStorageConnector connector;
	private ModuleInternal internal;

	public OwlapiStorageModule(Context context,
			PersistenceProvider persistenceProvider, DriverFactory factory)
			throws OntoDriverException {
		super(context, persistenceProvider, factory);
		this.internal = new ModuleInternalImpl(connector.getOntologyData(),
				this);
	}

	@Override
	public void close() throws OntoDriverException {
		factory.releaseStorageConnector(connector);
		this.internal = null;
		super.close();
	}

	@Override
	public void commit() throws OntoDriverException {
		ensureOpen();
		final List<OWLOntologyChange> changes = internal
				.commitAndRetrieveChanges();
		connector.applyChanges(changes);
		connector.saveWorkingOntology();
	}

	@Override
	public void rollback() throws OntoDriverException {
		ensureOpen();
		internal.rollback();
	}

	@Override
	protected void initialize() throws OntoDriverException {
		this.connector = (OwlapiStorageConnector) factory
				.createStorageConnector(context, false);
		if (!primaryKeyCounters.containsKey(context)) {
			primaryKeyCounters.put(context,
					new AtomicInteger(connector.getClassAssertionsCount()));
		}
	}

	@Override
	public boolean contains(Object primaryKey) throws OntoDriverException {
		ensureOpen();
		if (primaryKey == null) {
			throw new NullPointerException(
					"Null passed to contains: primaryKey = " + primaryKey);
		}
		return internal.containsEntity(primaryKey);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey)
			throws OntoDriverException {
		ensureOpen();
		if (cls == null || primaryKey == null) {
			throw new NullPointerException("Null passed to find: cls = " + cls
					+ ", primaryKey = " + primaryKey);
		}
		return internal.findEntity(cls, primaryKey);
	}

	@Override
	public <T> void loadFieldValue(T entity, String fieldName)
			throws OntoDriverException {
		ensureOpen();
		if (entity == null || fieldName == null) {
			throw new NullPointerException(
					"Null passed to loadFieldValues: entity = " + entity
							+ ", fieldName = " + fieldName);
		}
		internal.loadFieldValue(entity, fieldName);
	}

	@Override
	public <T> void merge(Object primaryKey, T entity)
			throws OntoDriverException {
		ensureOpen();
		if (primaryKey == null || entity == null) {
			throw new NullPointerException(
					"Null passed to merge: primaryKey = " + primaryKey
							+ ", entity = " + entity);
		}
		internal.mergeEntity(primaryKey, entity);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity)
			throws OntoDriverException {
		ensureOpen();
		if (entity == null) {
			throw new NullPointerException("Null passed to persist: entity = "
					+ entity);
		}
		internal.persistEntity(primaryKey, entity);
	}

	@Override
	public void remove(Object primaryKey) throws OntoDriverException {
		ensureOpen();
		if (primaryKey == null) {
			throw new NullPointerException(
					"Null passed to remove: primaryKey = " + primaryKey);
		}
		internal.removeEntity(primaryKey);
	}

	@Override
	public ResultSet executeStatement(Statement statement)
			throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * Returns facade to the persistence provider.
	 * 
	 * @return Persistence provider facade
	 */
	PersistenceProvider getPersistenceProvider() {
		return persistenceProvider;
	}

	/**
	 * Returns cloned ontology structures that can be manipulated without
	 * affecting the original data.
	 * 
	 * @return OwlapiConnectorDataHolder
	 * @throws OntoDriverException
	 *             If an error during cloning occurs
	 */
	OwlapiConnectorDataHolder cloneOntologyData() throws OntoDriverException {
		return connector.cloneOntologyData();
	}

	/**
	 * Returns the original ontology structures directly from the connector.
	 * 
	 * @return OwlapiConnectorDataHolder
	 */
	OwlapiConnectorDataHolder getOntologyData() {
		return connector.getOntologyData();
	}

	/**
	 * Retrieves a new primary key number and increments the internal counter.
	 * 
	 * @return primary key number
	 */
	int getNewPrimaryKey() {
		return StorageModule.getNewPrimaryKey(context);
	}

	/**
	 * Increments the primary key counter for this module's context.
	 */
	void incrementPrimaryKeyCounter() {
		StorageModule.incrementPrimaryKeyCounter(context);
	}
}
