package cz.cvut.kbss.ontodriver.impl.owlapi;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class OwlapiBasedCachingJenaModule extends OwlapiBasedJenaModule {

	private OwlapiConnectorDataHolder data;

	public OwlapiBasedCachingJenaModule(Repository repository,
			PersistenceProviderFacade persistenceProvider, DriverFactory factory)
			throws OntoDriverException {
		super(repository, persistenceProvider, factory);
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
		return data;
	}

	/**
	 * Returns the original ontology structures directly from the connector.
	 * 
	 * @return OwlapiConnectorDataHolder
	 * @throws OntoDriverException
	 */
	public OwlapiConnectorDataHolder getOntologyData() {
		try {
			this.data = connector.getOntologyDataInOwlapi();
			return data;
		} catch (OntoDriverException e) {
			throw new RuntimeException(e);
		}
	}
}
