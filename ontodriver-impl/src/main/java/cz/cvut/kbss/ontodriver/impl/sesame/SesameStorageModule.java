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
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isConsistent() throws OntoDriverException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void merge(Object primaryKey, T entity) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void persist(Object primaryKey, T entity) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void remove(Object primaryKey) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public ResultSet executeStatement(AbstractStatement statement) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
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
