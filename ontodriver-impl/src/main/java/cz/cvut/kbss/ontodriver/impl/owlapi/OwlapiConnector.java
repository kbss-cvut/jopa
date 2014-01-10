package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.List;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * Common interface for OWL API based storage connectors.
 * 
 * @author kidney
 * 
 */
interface OwlapiConnector {

	/**
	 * Saves the ontology changes.
	 * 
	 * @throws OntoDriverException
	 *             If an error during this operation occurs
	 */
	public void saveWorkingOntology() throws OntoDriverException;

	/**
	 * Applies the specified changes to the ontology. </p>
	 * 
	 * @param changes
	 *            The changes to apply
	 * @throws OntoDriverException
	 *             If an error during change application occurs
	 * @throws NullPointerException
	 *             If {@code changes} is {@code null}
	 */
	public void applyChanges(List<OWLOntologyChange> changes) throws OntoDriverException;

	/**
	 * Retrieves number of class assertions in the underlying ontology. </p>
	 * 
	 * This is useful for automatic primary key generation.
	 * 
	 * @return Number of class assertions
	 */
	public int getClassAssertionsCount();
}
