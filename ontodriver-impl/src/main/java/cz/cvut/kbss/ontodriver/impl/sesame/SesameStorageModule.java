package cz.cvut.kbss.ontodriver.impl.sesame;

import java.lang.reflect.Field;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import cz.cvut.kbss.ontodriver.AbstractStatement;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.ModuleInternal;

public class SesameStorageModule extends StorageModule {

	private SesameStorageConnector connector;
	private ModuleInternal<SesameChange, SesameStatement> internal;

	public SesameStorageModule(Context context, PersistenceProviderFacade persistenceProvider,
			DriverFactory factory) throws OntoDriverException {
		super(context, persistenceProvider, factory);
	}

	@Override
	public void commit() throws OntoDriverException {
		ensureOpen();
		ensureTransactionActive();
		this.transaction = TransactionState.COMMIT;
		final List<SesameChange> changes = internal.commitAndRetrieveChanges();
		connector.applyChanges(changes);
		this.transaction = TransactionState.NO;
	}

	@Override
	public void rollback() throws OntoDriverException {
		ensureOpen();
		internal.reset();
		this.transaction = TransactionState.NO;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		factory.releaseStorageConnector(connector);
		this.internal = null;
		super.close();
	}

	@Override
	protected void initialize() throws OntoDriverException {
		this.connector = (SesameStorageConnector) factory.createStorageConnector(context, false);
		if (!primaryKeyCounters.containsKey(context)) {
			primaryKeyCounters.put(context, new AtomicInteger());
		}
		this.internal = new SesameModuleInternal(connector.getModel());
	}

	@Override
	public boolean contains(Object primaryKey) throws OntoDriverException {
		beforeExecution();
		if (primaryKey == null) {
			throw new NullPointerException("PrimaryKey is null!");
		}
		return internal.containsEntity(primaryKey);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey) throws OntoDriverException {
		beforeExecution();
		if (cls == null || primaryKey == null) {
			throw new NullPointerException("Null passed to find: cls = " + cls + ", primaryKey = "
					+ primaryKey);
		}
		return internal.findEntity(cls, primaryKey);
	}

	@Override
	public boolean isConsistent() throws OntoDriverException {
		beforeExecution();
		return internal.isConsistent();
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException {
		beforeExecution();
		if (entity == null || field == null) {
			throw new NullPointerException("Null passed to loadFieldValue: entity = " + entity
					+ ", field = " + field);
		}
		internal.loadFieldValue(entity, field);
	}

	@Override
	public <T> void merge(Object primaryKey, T entity) throws OntoDriverException {
		beforeExecution();
		if (primaryKey == null || entity == null) {
			throw new NullPointerException("Null passed to merge: primaryKey = " + primaryKey
					+ ", entity = " + entity);
		}
		internal.mergeEntity(primaryKey, entity);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity) throws OntoDriverException {
		beforeExecution();
		if (entity == null) {
			throw new NullPointerException("Null passed to persist: entity = " + entity);
		}
		internal.persistEntity(primaryKey, entity);
	}

	@Override
	public void remove(Object primaryKey) throws OntoDriverException {
		beforeExecution();
		if (primaryKey == null) {
			throw new NullPointerException("Null passed to remove.");
		}
		internal.removeEntity(primaryKey);
	}

	@Override
	public ResultSet executeStatement(AbstractStatement statement) throws OntoDriverException {
		beforeExecution();
		if (statement == null) {
			throw new NullPointerException("Null passed to executeStatement.");
		}
		final SesameStatement stmt = (SesameStatement) factory.createStatement(statement);
		return internal.executeStatement(stmt);
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

	/**
	 * Performs maintenance tasks needed before execution of most methods.
	 * 
	 * @throws OntoDriverException
	 */
	private void beforeExecution() throws OntoDriverException {
		ensureOpen();
		startTransactionIfNotActive();
	}
}
