package cz.cvut.kbss.owlpersistence.ic.internalmodel;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObjectProperty;

public interface ClassParticipationConstraint extends
		ParticipationConstraintAnnotation {

	public OWLObjectProperty getPredicate();

	public OWLClass getObject();

	public Integer getMinRequired();

	public Integer getMaxPossible();

}
