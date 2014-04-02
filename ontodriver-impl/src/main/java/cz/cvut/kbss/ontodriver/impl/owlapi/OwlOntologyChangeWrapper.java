package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.Objects;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyChangeVisitor;
import org.semanticweb.owlapi.model.OWLOntologyChangeVisitorEx;

import cz.cvut.kbss.jopa.utils.ErrorUtils;

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
		this.ontology = Objects
				.requireNonNull(ontology, ErrorUtils.constructNPXMessage("ontology"));
		this.change = Objects.requireNonNull(change, ErrorUtils.constructNPXMessage("change"));
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
