package cz.cvut.kbss.owlpersistence.ic.generator;

import java.util.Set;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import cz.cvut.kbss.owlpersistence.ic.internalmodel.OWLPersistenceAnnotation;

public interface IntegrityConstraintParser {

	public Set<OWLPersistenceAnnotation> parse(final OWLAxiom axiom,
			final OWLReasoner ontology, final OWLOntology f)
			throws UnsupportedICException;

}
