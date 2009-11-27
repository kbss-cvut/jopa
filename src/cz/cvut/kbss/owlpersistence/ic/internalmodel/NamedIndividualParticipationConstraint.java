package cz.cvut.kbss.owlpersistence.ic.internalmodel;

import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;

public interface NamedIndividualParticipationConstraint extends
		ParticipationConstraintAnnotation {

	public OWLObjectProperty getPredicate();

	public OWLNamedIndividual getObject();

}
