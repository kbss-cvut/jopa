package cz.cvut.kbss.ontodriver.impl.owlapi;

import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;

/**
 * This class wraps the OWLAPI {@code AddAxiom} so we are able to set the
 * axiom's ontology. This is necessary for applying transactional changes to the
 * live ontology.
 * 
 * @author kidney
 * 
 */
final class AddAxiomWrapper extends AddAxiom implements OntologyMutable {

	private OWLOntology ontology;

	public AddAxiomWrapper(OWLOntology ont, OWLAxiom axiom) {
		super(ont, axiom);
		this.ontology = ont;
	}

	@Override
	public OWLOntology getOntology() {
		return ontology;
	}

	@Override
	public void setOntology(OWLOntology ontology) {
		this.ontology = ontology;
	}

}
