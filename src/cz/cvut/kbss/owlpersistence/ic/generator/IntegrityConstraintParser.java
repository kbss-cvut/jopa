package cz.cvut.kbss.owlpersistence.ic.generator;

import java.util.Set;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;

import cz.cvut.kbss.owlpersistence.ic.internalmodel.OWLPersistenceAnnotation;

public interface IntegrityConstraintParser {

	public Set<OWLPersistenceAnnotation> parse(final OWLAxiom axiom,
			final OWLOntology ontology) throws UnsupportedICException;

}
