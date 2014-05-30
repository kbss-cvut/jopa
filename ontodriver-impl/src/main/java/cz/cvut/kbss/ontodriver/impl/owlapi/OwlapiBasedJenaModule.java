package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.List;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.jena.OwlapiBasedJenaConnector;

public class OwlapiBasedJenaModule extends BaseOwlapiModule {

	protected OwlapiBasedJenaConnector connector;

	public OwlapiBasedJenaModule(PersistenceProviderFacade persistenceProvider,
			DriverFactory factory) throws OntoDriverException {
		super(persistenceProvider, factory);
	}

	@Override
	public void close() throws OntoDriverException {
		factory.releaseStorageConnector(connector);
		this.internal = null;
		this.open = false;
	}

	@Override
	public void commit() throws OntoDriverException {
		ensureOpen();
		ensureTransactionActive();
		this.transaction = TransactionState.COMMIT;
		final List<OWLOntologyChange> changes = internal.commitAndRetrieveChanges();
		connector.applyOntologyChanges(changes);
		connector.saveOntology();
		this.transaction = TransactionState.NO;
	}

	@Override
	protected void initialize() throws OntoDriverException {
		this.connector = (OwlapiBasedJenaConnector) factory.createStorageConnector(false);
		final OwlapiConnectorDataHolder holder = getOntologyData();
		setPrimaryKeyCounter(connector.getClassAssertionAxiomsCount());
		this.internal = new OwlapiModuleInternal(holder, this);
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
		internal.reset();
		this.transaction = TransactionState.ACTIVE;
	}
}
