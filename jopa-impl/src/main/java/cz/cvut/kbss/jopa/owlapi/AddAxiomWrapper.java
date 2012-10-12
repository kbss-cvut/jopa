package cz.cvut.kbss.jopa.owlapi;

import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;

/**
 * This class wraps the OWLAPI {@code AddAxiom} so we are able to set the
 * axiom's ontology. This is necessary for applying transactional changes to the
 * central ontology.
 * 
 * @author kidney
 * 
 */
public class AddAxiomWrapper extends AddAxiom implements OntologyChangeWrapper {

	private OWLOntology ontology;

	public AddAxiomWrapper(OWLOntology ont, OWLAxiom axiom) {
		super(ont, axiom);
		this.ontology = ont;
	}

	@Override
	public OWLOntology getOntology() {
		return ontology;
	}

	public void setOntology(OWLOntology ontology) {
		this.ontology = ontology;
	}

}
