package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.List;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverInitializationException;

public class ModularizingOwlapiStorageModule extends BaseOwlapiModule {

	private ModularizingStorageConnector connector;

	private OwlapiConnectorDataHolder data;

	public ModularizingOwlapiStorageModule(PersistenceProviderFacade persistenceProvider,
			DriverFactory factory) throws OntoDriverException {
		super(persistenceProvider, factory);
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
	protected void initialize() throws OntoDriverException {
		this.connector = (ModularizingStorageConnector) factory.createStorageConnector();
		setPrimaryKeyCounter(connector.getClassAssertionsCount());
		this.data = connector.extractOntologyModule(getMetamodel());
		this.internal = new OwlapiModuleInternal(data, this);
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
	protected void startTransactionIfNotActive() throws OntoDriverException {
		if (transaction == TransactionState.ACTIVE) {
			return;
		}
		connector.reload();
		internal.reset();
		this.transaction = TransactionState.ACTIVE;
	}
}
