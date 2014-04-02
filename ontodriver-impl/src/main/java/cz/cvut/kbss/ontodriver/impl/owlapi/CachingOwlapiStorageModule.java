package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.lang.reflect.Field;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverInitializationException;
import cz.cvut.kbss.ontodriver.impl.ModuleInternal;

public class CachingOwlapiStorageModule extends StorageModule implements OwlapiModuleWrapper {

	private CachingOwlapiStorageConnector connector;
	private ModuleInternal<OWLOntologyChange, OwlapiStatement> internal;

	private OwlapiConnectorDataHolder data;

	public CachingOwlapiStorageModule(Repository repository,
			PersistenceProviderFacade persistenceProvider, DriverFactory factory)
			throws OntoDriverException {
		super(repository, persistenceProvider, factory);
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
		this.connector = (CachingOwlapiStorageConnector) factory.createStorageConnector(
				repositoryId, false);
		if (!primaryKeyCounters.containsKey(repository.getId())) {
			primaryKeyCounters.put(repository.getId(),
					new AtomicInteger(connector.getClassAssertionsCount()));
		}
		this.internal = new OwlapiModuleInternal(getOntologyData(), this);
	}

	@Override
	public boolean contains(Object primaryKey, RepositoryID contexts) throws OntoDriverException {
		preContains(primaryKey, contexts);

		return internal.containsEntity(primaryKey, contexts);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, RepositoryID contexts)
			throws OntoDriverException {
		preFind(cls, primaryKey, contexts);

		return internal.findEntity(cls, primaryKey, contexts);
	}

	@Override
	public boolean isConsistent(RepositoryID contexts) throws OntoDriverException {
		preIsConsistent(contexts);

		return internal.isConsistent(contexts);
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, RepositoryID contexts)
			throws OntoDriverException {
		preLoadFieldValue(entity, field, contexts);

		internal.loadFieldValue(entity, field, contexts);
	}

	@Override
	public <T> void merge(Object primaryKey, T entity, Field mergedField, RepositoryID context)
			throws OntoDriverException {
		preMerge(primaryKey, entity, mergedField, context);

		internal.mergeEntity(primaryKey, entity, mergedField, context);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity, RepositoryID context)
			throws OntoDriverException {
		prePersist(entity, context);

		internal.persistEntity(primaryKey, entity, context);
	}

	@Override
	public void remove(Object primaryKey, RepositoryID context) throws OntoDriverException {
		preRemove(primaryKey, context);

		internal.removeEntity(primaryKey, context);
	}

	@Override
	public ResultSet executeStatement(JopaStatement statement) throws OntoDriverException {
		ensureOpen();
		startTransactionIfNotActive();
		final OwlapiStatement stmt = (OwlapiStatement) factory.createStatement(statement);
		return internal.executeStatement(stmt);
	}

	@Override
	public PersistenceProviderFacade getPersistenceProvider() {
		return persistenceProvider;
	}

	@Override
	public OwlapiConnectorDataHolder cloneOntologyData() throws OntoDriverException {
		// No need to clone, the data is already cloned
		assert data != null;
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
	public RepositoryID getRepositoryIdentifier() {
		return repositoryId;
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
