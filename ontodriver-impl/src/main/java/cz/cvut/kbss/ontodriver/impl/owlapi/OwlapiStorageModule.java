package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.List;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class OwlapiStorageModule extends BaseOwlapiModule {

	OwlapiStorageConnector connector;

	public OwlapiStorageModule(PersistenceProviderFacade persistenceProvider, DriverFactory factory)
			throws OntoDriverException {
		super(persistenceProvider, factory);
	}

	@Override
	protected void initialize() throws OntoDriverException {
		this.connector = (OwlapiStorageConnector) factory.createStorageConnector();
		setPrimaryKeyCounter(connector.getClassAssertionsCount());
		this.internal = new OwlapiModuleInternal(connector.getOntologyData(), this);
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
	protected void startTransactionIfNotActive() throws OntoDriverException {
		if (transaction == TransactionState.ACTIVE) {
			return;
		}
		connector.reload();
		internal.reset();
		this.transaction = TransactionState.ACTIVE;
	}
}
