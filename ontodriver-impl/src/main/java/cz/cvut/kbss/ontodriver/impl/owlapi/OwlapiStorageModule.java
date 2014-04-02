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
import cz.cvut.kbss.ontodriver.impl.ModuleInternal;

public class OwlapiStorageModule extends StorageModule implements OwlapiModuleWrapper {

	private OwlapiStorageConnector connector;
	private ModuleInternal<OWLOntologyChange, OwlapiStatement> internal;

	public OwlapiStorageModule(Repository repository,
			PersistenceProviderFacade persistenceProvider, DriverFactory factory)
			throws OntoDriverException {
		super(repository, persistenceProvider, factory);
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
		this.connector = (OwlapiStorageConnector) factory.createStorageConnector(repositoryId,
				false);
		if (!primaryKeyCounters.containsKey(repository.getId())) {
			primaryKeyCounters.put(repository.getId(),
					new AtomicInteger(connector.getClassAssertionsCount()));
		}
		this.internal = new OwlapiModuleInternal(connector.getOntologyData(), this);
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
	public <T> void loadFieldValue(T entity, Field field, RepositoryID context)
			throws OntoDriverException {
		preLoadFieldValue(entity, field, context);
		internal.loadFieldValue(entity, field, context);
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

	/**
	 * Returns facade to the persistence provider.
	 * 
	 * @return Persistence provider facade
	 */
	public PersistenceProviderFacade getPersistenceProvider() {
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
	public OwlapiConnectorDataHolder cloneOntologyData() throws OntoDriverException {
		return connector.cloneOntologyData();
	}

	/**
	 * Returns the original ontology structures directly from the connector.
	 * 
	 * @return OwlapiConnectorDataHolder
	 */
	public OwlapiConnectorDataHolder getOntologyData() {
		return connector.getOntologyData();
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
