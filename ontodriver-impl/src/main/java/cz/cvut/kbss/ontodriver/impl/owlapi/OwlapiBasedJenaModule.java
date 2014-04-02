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
		this.connector = (OwlapiBasedJenaConnector) factory.createStorageConnector(repositoryId,
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
	public <T> T find(Class<T> cls, Object primaryKey, RepositoryID contexts)
			throws OntoDriverException {
		preFind(cls, primaryKey, contexts);

		return moduleInternal.findEntity(cls, primaryKey, contexts);
	}

	@Override
	public boolean isConsistent(RepositoryID contexts) throws OntoDriverException {
		preIsConsistent(contexts);

		return moduleInternal.isConsistent(contexts);
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, RepositoryID context)
			throws OntoDriverException {
		preLoadFieldValue(entity, field, context);

		moduleInternal.loadFieldValue(entity, field, context);
	}

	@Override
	public <T> void merge(Object primaryKey, T entity, Field mergedField, RepositoryID context)
			throws OntoDriverException {
		preMerge(primaryKey, entity, mergedField, context);

		moduleInternal.mergeEntity(primaryKey, entity, mergedField, context);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity, RepositoryID context)
			throws OntoDriverException {
		prePersist(entity, context);

		moduleInternal.persistEntity(primaryKey, entity, context);
	}

	@Override
	public void remove(Object primaryKey, RepositoryID context) throws OntoDriverException {
		preRemove(primaryKey, context);

		moduleInternal.removeEntity(primaryKey, context);
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
