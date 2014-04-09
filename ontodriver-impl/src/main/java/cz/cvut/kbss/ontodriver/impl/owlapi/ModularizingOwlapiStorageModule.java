package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverInitializationException;
import cz.cvut.kbss.ontodriver.impl.ModuleInternal;

public class ModularizingOwlapiStorageModule extends StorageModule implements OwlapiModuleWrapper {

	private ModularizingStorageConnector connector;
	private ModuleInternal<OWLOntologyChange, OwlapiStatement> internal;

	private OwlapiConnectorDataHolder data;

	public ModularizingOwlapiStorageModule(Repository repository,
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
		this.connector = (ModularizingStorageConnector) factory.createStorageConnector(repository,
				false);
		if (!primaryKeyCounters.containsKey(repository.getId())) {
			primaryKeyCounters.put(repository.getId(),
					new AtomicInteger(connector.getClassAssertionsCount()));
		}
		this.data = connector.extractOntologyModule(getMetamodel());
		this.internal = new OwlapiModuleInternal(data, this);
	}

	@Override
	public boolean contains(Object primaryKey, RepositoryID contexts) throws OntoDriverException {
		preContains(primaryKey, contexts);

		return internal.containsEntity(primaryKey, contexts);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, EntityDescriptor descriptor)
			throws OntoDriverException {
		preFind(cls, primaryKey, descriptor);

		return internal.findEntity(cls, primaryKey, descriptor);
	}

	@Override
	public boolean isConsistent(RepositoryID contexts) throws OntoDriverException {
		preIsConsistent(contexts);

		return internal.isConsistent(contexts);
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, EntityDescriptor descriptor)
			throws OntoDriverException {
		preLoadFieldValue(entity, field, descriptor);

		internal.loadFieldValue(entity, field, descriptor);
	}

	@Override
	public <T> void merge(T entity, Field mergedField, EntityDescriptor descriptor)
			throws OntoDriverException {
		preMerge(entity, mergedField, descriptor);

		internal.mergeEntity(entity, mergedField, descriptor);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity, EntityDescriptor descriptor)
			throws OntoDriverException {
		prePersist(entity, descriptor);
		internal.persistEntity(primaryKey, entity, descriptor);
	}

	@Override
	public void remove(Object primaryKey, EntityDescriptor descriptor) throws OntoDriverException {
		preRemove(primaryKey, descriptor);

		internal.removeEntity(primaryKey, descriptor);
	}

	@Override
	public ResultSet executeStatement(JopaStatement statement) throws OntoDriverException {
		ensureOpen();
		startTransactionIfNotActive();
		final OwlapiStatement stmt = (OwlapiStatement) factory.createStatement(statement);
		return internal.executeStatement(stmt);
	}

	@Override
	public OwlapiConnectorDataHolder cloneOntologyData() throws OntoDriverException {
		assert data != null;
		return data;
	}

	@Override
	public OwlapiConnectorDataHolder getOntologyData() {
		try {
			this.data = connector.extractOntologyModule(getMetamodel());
		} catch (OntoDriverException e) {
			throw new OntoDriverInitializationException(e);
		}
		return data;
	}

	@Override
	public <T> T getEntityFromCache(Class<T> cls, Object primaryKey) {
		Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		final EntityDescriptor descriptor = new EntityDescriptor(repository);
		// OWLAPI repositories have their logical URI as the only context
		if (!repository.getContexts().isEmpty()) {
			descriptor.setEntityContext(repository.getContexts().iterator().next());
		}

		return getEntityFromProviderCache(cls, primaryKey, descriptor);
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
