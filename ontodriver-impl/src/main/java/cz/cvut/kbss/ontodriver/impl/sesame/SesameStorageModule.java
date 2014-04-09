package cz.cvut.kbss.ontodriver.impl.sesame;

import java.lang.reflect.Field;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.ModuleInternal;

public class SesameStorageModule extends StorageModule {

	private SesameStorageConnector connector;
	private ModuleInternal<SesameChange, SesameStatement> internal;

	public SesameStorageModule(Repository repository,
			PersistenceProviderFacade persistenceProvider, DriverFactory factory)
			throws OntoDriverException {
		super(repository, persistenceProvider, factory);
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
		internal.rollback();
		this.transaction = TransactionState.NO;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		internal.rollback();
		this.internal = null;
		factory.releaseStorageConnector(connector);
		super.close();
	}

	@Override
	protected void initialize() throws OntoDriverException {
		this.connector = (SesameStorageConnector) factory.createStorageConnector(repositoryId,
				false);
		this.internal = createModuleInternal();
		if (!primaryKeyCounters.containsKey(repository)) {
			primaryKeyCounters.put(repository.getId(),
					new AtomicInteger((int) connector.getSubjectCount()));
		}
	}

	@Override
	public boolean contains(Object primaryKey, EntityDescriptor contexts) throws OntoDriverException {
		preContains(primaryKey, contexts);

		return internal.containsEntity(primaryKey, contexts);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, EntityDescriptor contexts)
			throws OntoDriverException {
		preFind(cls, primaryKey, contexts);

		return internal.findEntity(cls, primaryKey, contexts);
	}

	@Override
	public boolean isConsistent(EntityDescriptor contexts) throws OntoDriverException {
		preIsConsistent(contexts);

		return internal.isConsistent(contexts);
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, EntityDescriptor context)
			throws OntoDriverException {
		preLoadFieldValue(entity, field, context);

		internal.loadFieldValue(entity, field, context);
	}

	@Override
	public <T> void merge(T entity, Field mergedField, EntityDescriptor context)
			throws OntoDriverException {
		preMerge(entity, mergedField, context);

		internal.mergeEntity(entity, mergedField, context);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity, EntityDescriptor context)
			throws OntoDriverException {
		prePersist(entity, context);

		internal.persistEntity(primaryKey, entity, context);
	}

	@Override
	public void remove(Object primaryKey, EntityDescriptor context) throws OntoDriverException {
		preRemove(primaryKey, context);

		internal.removeEntity(primaryKey, context);
	}

	@Override
	public ResultSet executeStatement(JopaStatement statement) throws OntoDriverException {
		ensureOpen();
		startTransactionIfNotActive();
		final SesameStatement stmt = (SesameStatement) factory.createStatement(statement);
		return internal.executeStatement(stmt);
	}

	@Override
	protected void startTransactionIfNotActive() throws OntoDriverException {
		if (transaction == TransactionState.ACTIVE) {
			return;
		}
		internal.reset();
		this.transaction = TransactionState.ACTIVE;
	}

	SesameOntologyDataHolder getOntologyData() throws OntoDriverException {
		return connector.getOntologyData();
	}

	/**
	 * Creates {@link ModuleInternal} implementation appropriate for this
	 * storage module.
	 * 
	 * @return {@code ModuleInternal}
	 * @throws OntoDriverException
	 *             If we are unable to create the instance
	 */
	protected ModuleInternal<SesameChange, SesameStatement> createModuleInternal()
			throws OntoDriverException {
		// Can add more internal implementations here
		return new SesameModuleInternal(getOntologyData(), this);
	}
}
