package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.lang.reflect.Field;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverInitializationException;

public class CachingOwlapiStorageModule extends StorageModule implements OwlapiModuleWrapper {

	private CachingOwlapiStorageConnector connector;
	private ModuleInternal internal;

	private OwlapiConnectorDataHolder data;

	public CachingOwlapiStorageModule(Context context, PersistenceProviderFacade persistenceProvider,
			DriverFactory factory) throws OntoDriverException {
		super(context, persistenceProvider, factory);
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
		final List<OWLOntologyChange> changes = internal.commitAndRetrieveChanges();
		connector.applyChanges(changes);
		connector.saveWorkingOntology();
		// Necessary to maintain up to-date data
		internal.reset();
	}

	@Override
	public void rollback() throws OntoDriverException {
		ensureOpen();
		internal.rollback();
	}

	@Override
	protected void initialize() throws OntoDriverException {
		this.connector = (CachingOwlapiStorageConnector) factory.createStorageConnector(context,
				false);
		if (!primaryKeyCounters.containsKey(context)) {
			primaryKeyCounters.put(context, new AtomicInteger(connector.getClassAssertionsCount()));
		}
		this.internal = new ModuleInternalImpl(getOntologyData(), this);
	}

	@Override
	public boolean contains(Object primaryKey) throws OntoDriverException {
		ensureOpen();
		if (primaryKey == null) {
			throw new NullPointerException("Null passed to contains: primaryKey = " + primaryKey);
		}
		return internal.containsEntity(primaryKey);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey) throws OntoDriverException {
		ensureOpen();
		if (cls == null || primaryKey == null) {
			throw new NullPointerException("Null passed to find: cls = " + cls + ", primaryKey = "
					+ primaryKey);
		}
		return internal.findEntity(cls, primaryKey);
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException {
		ensureOpen();
		if (entity == null || field == null) {
			throw new NullPointerException("Null passed to loadFieldValues: entity = " + entity
					+ ", fieldName = " + field);
		}
		internal.loadFieldValue(entity, field);
	}

	@Override
	public <T> void merge(Object primaryKey, T entity) throws OntoDriverException {
		ensureOpen();
		if (primaryKey == null || entity == null) {
			throw new NullPointerException("Null passed to merge: primaryKey = " + primaryKey
					+ ", entity = " + entity);
		}
		internal.mergeEntity(primaryKey, entity);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity) throws OntoDriverException {
		ensureOpen();
		if (entity == null) {
			throw new NullPointerException("Null passed to persist: entity = " + entity);
		}
		internal.persistEntity(primaryKey, entity);
	}

	@Override
	public void remove(Object primaryKey) throws OntoDriverException {
		ensureOpen();
		if (primaryKey == null) {
			throw new NullPointerException("Null passed to remove: primaryKey = " + primaryKey);
		}
		internal.removeEntity(primaryKey);
	}

	@Override
	public ResultSet executeStatement(Statement statement) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public PersistenceProviderFacade getPersistenceProvider() {
		return persistenceProvider;
	}

	@Override
	public OwlapiConnectorDataHolder cloneOntologyData() throws OntoDriverException {
		// No need to clone, the data is already cloned
		return data;
	}

	@Override
	public OwlapiConnectorDataHolder getOntologyData() {
		try {
			this.data = connector.getOntologyData();
		} catch (OntoDriverException e) {
			throw new OntoDriverInitializationException(e);
		}
		return data;
	}

	@Override
	public int getNewPrimaryKey() {
		return StorageModule.getNewPrimaryKey(context);
	}

	@Override
	public void incrementPrimaryKeyCounter() {
		StorageModule.incrementPrimaryKeyCounter(context);
	}
}
