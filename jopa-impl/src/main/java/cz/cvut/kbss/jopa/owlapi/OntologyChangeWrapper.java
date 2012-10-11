package cz.cvut.kbss.jopa.owlapi;

import org.semanticweb.owlapi.model.OWLOntology;

public interface OntologyChangeWrapper {

	/**
	 * Set ontology of this change. </p>
	 * 
	 * Setting the ontology is necessary for correct change application, since
	 * the transactional changes have to be after commit applied to the central
	 * ontology.
	 * 
	 * @param ontology
	 */
	public void setOntology(OWLOntology ontology);

}
