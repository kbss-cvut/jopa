package cz.cvut.kbss.owlpersistence.owl2java;

import java.util.Set;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import cz.cvut.kbss.owlpersistence.model.ic.IntegrityConstraint;

public interface ICEvaluator {

	public Set<IntegrityConstraint> parse(final OWLAxiom axiom,
			final OWLReasoner ontology, final OWLOntology f)
			throws UnsupportedICException;

}
