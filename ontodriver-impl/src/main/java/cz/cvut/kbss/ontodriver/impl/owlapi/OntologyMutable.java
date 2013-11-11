package cz.cvut.kbss.ontodriver.impl.owlapi;

import org.semanticweb.owlapi.model.OWLOntology;

public interface OntologyMutable {

	/**
	 * Sets ontology on this object. </p>
	 * 
	 * @param ontology
	 *            The ontology to set
	 * @throws NullPointerException
	 *             If the ontology is null
	 */
	public void setOntology(OWLOntology ontology);

	/**
	 * Retrieves this object's current ontology;
	 * 
	 * @return OWLOntology
	 */
	public OWLOntology getOntology();
}
