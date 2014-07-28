package cz.cvut.kbss.ontodriver.impl.sesame;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
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

	public SesameStorageModule(PersistenceProviderFacade persistenceProvider, DriverFactory factory)
			throws OntoDriverException {
		super(persistenceProvider, factory);
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
		this.connector = (SesameStorageConnector) factory.createStorageConnector();
		setPrimaryKeyCounter((int) connector.getSubjectCount());
		this.internal = createModuleInternal();
	}

	@Override
	public boolean contains(Object primaryKey, URI context) throws OntoDriverException {
		preContains(primaryKey, context);

		return internal.containsEntity(primaryKey, context);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, Descriptor descriptor)
			throws OntoDriverException {
		preFind(cls, primaryKey, descriptor);

		return internal.findEntity(cls, primaryKey, descriptor);
	}

	@Override
	public boolean isConsistent(URI context) throws OntoDriverException {
		preIsConsistent(context);

		return internal.isConsistent(context);
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, Descriptor descriptor)
			throws OntoDriverException {
		preLoadFieldValue(entity, field, descriptor);

		internal.loadFieldValue(entity, field, descriptor);
	}

	@Override
	public <T> void merge(T entity, Field mergedField, Descriptor descriptor)
			throws OntoDriverException {
		preMerge(entity, mergedField, descriptor);

		internal.mergeEntity(entity, mergedField, descriptor);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity, Descriptor descriptor)
			throws OntoDriverException {
		prePersist(entity, descriptor);

		internal.persistEntity(primaryKey, entity, descriptor);
	}

	@Override
	public void remove(Object primaryKey, Descriptor descriptor) throws OntoDriverException {
		preRemove(primaryKey, descriptor);

		internal.removeEntity(primaryKey, descriptor);
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

	@Override
	public List<URI> getContexts() {
		ensureOpen();
		return internal.getContexts();
	}
}
