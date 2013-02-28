package cz.cvut.kbss.ontodriver.impl.owlapi;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyChangeVisitor;
import org.semanticweb.owlapi.model.OWLOntologyChangeVisitorEx;

/**
 * Wraps standard OWLAPI ontology change and adds possibility to set ontology
 * for the change.
 * 
 * @author kidney
 * 
 */
class OwlOntologyChangeWrapper extends OWLOntologyChange implements OntologyMutable {

	private final OWLOntologyChange change;
	private OWLOntology ontology;

	public OwlOntologyChangeWrapper(OWLOntology ontology, OWLOntologyChange change) {
		super(ontology);
		if (ontology == null) {
			throw new NullPointerException("Null passed as ontology to OWLOntologyWrapper.");
		}
		if (change == null) {
			throw new NullPointerException("Null passed as change to OWLOntologyWrapper.");
		}
		this.change = change;
		this.ontology = ontology;
	}

	@Override
	public boolean isAxiomChange() {
		return change.isAxiomChange();
	}

	@Override
	public OWLAxiom getAxiom() {
		return change.getAxiom();
	}

	@Override
	public boolean isImportChange() {
		return change.isImportChange();
	}

	@Override
	public void accept(OWLOntologyChangeVisitor visitor) {
		change.accept(visitor);

	}

	@Override
	public <O> O accept(OWLOntologyChangeVisitorEx<O> visitor) {
		return change.accept(visitor);
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
