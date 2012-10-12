package cz.cvut.kbss.jopa.owlapi;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.RemoveAxiom;

public class RemoveAxiomWrapper extends RemoveAxiom implements
		OntologyChangeWrapper {

	private OWLOntology ontology;

	public RemoveAxiomWrapper(OWLOntology ont, OWLAxiom axiom) {
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
