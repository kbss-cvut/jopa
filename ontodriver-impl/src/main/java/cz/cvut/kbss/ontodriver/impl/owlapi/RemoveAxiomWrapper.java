package cz.cvut.kbss.ontodriver.impl.owlapi;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.RemoveAxiom;

public class RemoveAxiomWrapper extends RemoveAxiom implements OntologyMutable {

	private OWLOntology ontology;

	public RemoveAxiomWrapper(OWLOntology ont, OWLAxiom axiom) {
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
