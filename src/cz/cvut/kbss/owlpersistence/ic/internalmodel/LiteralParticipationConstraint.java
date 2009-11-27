package cz.cvut.kbss.owlpersistence.ic.internalmodel;

import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLLiteral;

public interface LiteralParticipationConstraint extends
		ParticipationConstraintAnnotation {

	public OWLDataProperty getPredicate();

	public OWLLiteral getObject();

}
