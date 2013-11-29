package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.lang.reflect.Field;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.ontodriver.AbstractStatement;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverInitializationException;

public class ModularizingOwlapiStorageModule extends StorageModule implements OwlapiModuleWrapper {

	private ModularizingStorageConnector connector;
	private OwlapiModuleInternal internal;

	private OwlapiConnectorDataHolder data;

	public ModularizingOwlapiStorageModule(Context context,
			PersistenceProviderFacade persistenceProvider, DriverFactory factory)
			throws OntoDriverException {
		super(context, persistenceProvider, factory);
	}

	@Override
	public void close() throws OntoDriverException {
		factory.releaseStorageConnector(connector);
		internal.rollback();
		this.internal = null;
		this.data = null;
		super.close();
	}

	@Override
	public void commit() throws OntoDriverException {
		ensureOpen();
		ensureTransactionActive();
		this.transaction = TransactionState.COMMIT;
		final List<OWLOntologyChange> changes = internal.commitAndRetrieveChanges();
		connector.applyChanges(changes);
		connector.saveWorkingOntology();
		this.transaction = TransactionState.NO;
	}

	@Override
	public void rollback() throws OntoDriverException {
		ensureOpen();
		internal.rollback();
		this.transaction = TransactionState.NO;
	}

	@Override
	protected void initialize() throws OntoDriverException {
		this.connector = (ModularizingStorageConnector) factory.createStorageConnector(context,
				false);
		if (!primaryKeyCounters.containsKey(context)) {
			primaryKeyCounters.put(context, new AtomicInteger(connector.getClassAssertionsCount()));
		}
		this.data = connector.extractOntologyModule(getMetamodel(), context);
		this.internal = new ModuleInternalImpl(data, this);
	}

	@Override
	public boolean contains(Object primaryKey) throws OntoDriverException {
		ensureOpen();
		startTransactionIfNotActive();
		if (primaryKey == null) {
			throw new NullPointerException("Null passed to contains: primaryKey = " + primaryKey);
		}
		return internal.containsEntity(primaryKey);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey) throws OntoDriverException {
		ensureOpen();
		startTransactionIfNotActive();
		if (cls == null || primaryKey == null) {
			throw new NullPointerException("Null passed to find: cls = " + cls + ", primaryKey = "
					+ primaryKey);
		}
		return internal.findEntity(cls, primaryKey);
	}

	@Override
	public boolean isConsistent() throws OntoDriverException {
		ensureOpen();
		startTransactionIfNotActive();
		return internal.isConsistent();
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException {
		ensureOpen();
		startTransactionIfNotActive();
		if (entity == null || field == null) {
			throw new NullPointerException("Null passed to loadFieldValues: entity = " + entity
					+ ", fieldName = " + field);
		}
		internal.loadFieldValue(entity, field);
	}

	@Override
	public <T> void merge(Object primaryKey, T entity) throws OntoDriverException {
		ensureOpen();
		startTransactionIfNotActive();
		if (primaryKey == null || entity == null) {
			throw new NullPointerException("Null passed to merge: primaryKey = " + primaryKey
					+ ", entity = " + entity);
		}
		internal.mergeEntity(primaryKey, entity);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity) throws OntoDriverException {
		ensureOpen();
		startTransactionIfNotActive();
		if (entity == null) {
			throw new NullPointerException("Null passed to persist: entity = " + entity);
		}
		internal.persistEntity(primaryKey, entity);
	}

	@Override
	public void remove(Object primaryKey) throws OntoDriverException {
		ensureOpen();
		startTransactionIfNotActive();
		if (primaryKey == null) {
			throw new NullPointerException("Null passed to remove: primaryKey = " + primaryKey);
		}
		internal.removeEntity(primaryKey);
	}

	@Override
	public ResultSet executeStatement(AbstractStatement statement) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public PersistenceProviderFacade getPersistenceProvider() {
		return persistenceProvider;
	}

	@Override
	public OwlapiConnectorDataHolder cloneOntologyData() throws OntoDriverException {
		assert data != null;
		return data;
	}

	@Override
	public OwlapiConnectorDataHolder getOntologyData() {
		try {
			this.data = connector.extractOntologyModule(getMetamodel(), context);
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

	@Override
	protected void startTransactionIfNotActive() throws OntoDriverException {
		if (transaction == TransactionState.ACTIVE) {
			return;
		}
		connector.reload();
		internal.reset();
		this.transaction = TransactionState.ACTIVE;
	}
}
