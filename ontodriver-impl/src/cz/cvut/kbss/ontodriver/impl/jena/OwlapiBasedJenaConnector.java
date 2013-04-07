package cz.cvut.kbss.ontodriver.impl.jena;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import cz.cvut.kbss.ontodriver.StorageConnector;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiConnectorDataHolder;

/**
 * Common interface for OWL API based Jena storage connectors.
 * 
 * @author kidney
 * 
 */
public interface OwlapiBasedJenaConnector extends StorageConnector {

	/**
	 * Retrieves the ontology in OWL API structures. </p>
	 * 
	 * This method internally performs a transformation from Jena model to OWL
	 * API data structures so that OWL API internals can be used to work with
	 * the data.
	 * 
	 * @return {@code OwlapiConnectorDataHolder}
	 * @throws OntoDriverException
	 *             If an error during transformation occurs
	 */
	public OwlapiConnectorDataHolder getOntologyDataInOwlapi() throws OntoDriverException;

	/**
	 * Returns the number of class assertion axioms in the working ontology.
	 * 
	 * @return Number of axioms or 0 if there are none
	 */
	public int getClassAssertionAxiomsCount();

	/**
	 * Returns actually the current ontology data, since there is no need to
	 * clone them because the main ontology is stored as Jena model.
	 * 
	 * @return {@code OwlapiConnectorDataHolder}
	 */
	public OwlapiConnectorDataHolder cloneOntologyDataInOwlapi() throws OntoDriverException;

	/**
	 * Applies changes made to the {@code ontology} to the Jena ontology model
	 * held by this connector. </p>
	 * 
	 * Note that by calling this method the original model held by this
	 * connector is closed and recreated from the specified {@code ontology}.
	 * </p>
	 * 
	 * Also note that calling this method does not save the changes into the
	 * underlying storage, this has to be done by explicitly calling
	 * {@link #saveOntology()}.
	 * 
	 * @param manager
	 *            {@code OWLOntologyManager}
	 * @param ontology
	 *            {@code OWLOntology} containing changes
	 * @throws OntoDriverException
	 *             IF an error during OWL API -> Jena transformation occurs
	 */
	public void applyOntologyChanges(OWLOntologyManager manager, OWLOntology ontology)
			throws OntoDriverException;

	/**
	 * Outputs the ontology into its physical location.
	 * 
	 * @throws OntoDriverException
	 */
	public void saveOntology() throws OntoDriverException;
}
