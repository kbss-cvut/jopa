package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.lang.reflect.Field;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.ModuleInternal;
import cz.cvut.kbss.ontodriver.impl.jena.OwlapiBasedJenaConnector;

public class OwlapiBasedJenaModule extends OwlapiStorageModule {

	protected OwlapiBasedJenaConnector connector;
	protected ModuleInternal<OWLOntologyChange, OwlapiStatement> moduleInternal;

	public OwlapiBasedJenaModule(Repository repository,
			PersistenceProviderFacade persistenceProvider, DriverFactory factory)
			throws OntoDriverException {
		super(repository, persistenceProvider, factory);
	}

	@Override
	public void close() throws OntoDriverException {
		factory.releaseStorageConnector(connector);
		this.moduleInternal = null;
		this.open = false;
	}

	@Override
	public void commit() throws OntoDriverException {
		ensureOpen();
		ensureTransactionActive();
		this.transaction = TransactionState.COMMIT;
		final List<OWLOntologyChange> changes = moduleInternal.commitAndRetrieveChanges();
		connector.applyOntologyChanges(changes);
		connector.saveOntology();
		this.transaction = TransactionState.NO;
	}

	@Override
	public void rollback() throws OntoDriverException {
		ensureOpen();
		moduleInternal.rollback();
		this.transaction = TransactionState.NO;
	}

	@Override
	protected void initialize() throws OntoDriverException {
		this.connector = (OwlapiBasedJenaConnector) factory.createStorageConnector(repository,
				false);
		final OwlapiConnectorDataHolder holder = getOntologyData();
		if (!primaryKeyCounters.containsKey(repository.getId())) {
			primaryKeyCounters.put(repository.getId(),
					new AtomicInteger(connector.getClassAssertionAxiomsCount()));
		}
		this.moduleInternal = new OwlapiModuleInternal(holder, this);
	}

	@Override
	public boolean contains(Object primaryKey, RepositoryID contexts) throws OntoDriverException {
		preContains(primaryKey, contexts);

		return moduleInternal.containsEntity(primaryKey, contexts);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, EntityDescriptor descriptor)
			throws OntoDriverException {
		preFind(cls, primaryKey, descriptor);

		return moduleInternal.findEntity(cls, primaryKey, descriptor);
	}

	@Override
	public boolean isConsistent(RepositoryID contexts) throws OntoDriverException {
		preIsConsistent(contexts);

		return moduleInternal.isConsistent(contexts);
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, EntityDescriptor descriptor)
			throws OntoDriverException {
		preLoadFieldValue(entity, field, descriptor);

		moduleInternal.loadFieldValue(entity, field, descriptor);
	}

	@Override
	public <T> void merge(T entity, Field mergedField, EntityDescriptor descriptor)
			throws OntoDriverException {
		preMerge(entity, mergedField, descriptor);

		moduleInternal.mergeEntity(entity, mergedField, descriptor);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity, EntityDescriptor descriptor)
			throws OntoDriverException {
		prePersist(entity, descriptor);

		moduleInternal.persistEntity(primaryKey, entity, descriptor);
	}

	@Override
	public void remove(Object primaryKey, EntityDescriptor descriptor) throws OntoDriverException {
		preRemove(primaryKey, descriptor);

		moduleInternal.removeEntity(primaryKey, descriptor);
	}

	@Override
	public ResultSet executeStatement(JopaStatement statement) throws OntoDriverException {
		ensureOpen();
		startTransactionIfNotActive();
		final OwlapiStatement stmt = (OwlapiStatement) factory.createStatement(statement);
		return moduleInternal.executeStatement(stmt);
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
		return connector.cloneOntologyDataInOwlapi();
	}

	/**
	 * Returns the original ontology structures directly from the connector.
	 * 
	 * @return OwlapiConnectorDataHolder
	 * @throws OntoDriverException
	 */
	public OwlapiConnectorDataHolder getOntologyData() {
		try {
			final OwlapiConnectorDataHolder holder = connector.getOntologyDataInOwlapi();
			return holder;
		} catch (OntoDriverException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	protected void startTransactionIfNotActive() throws OntoDriverException {
		if (transaction == TransactionState.ACTIVE) {
			return;
		}
		connector.reload();
		moduleInternal.reset();
		this.transaction = TransactionState.ACTIVE;
	}
}
