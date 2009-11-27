package cz.cvut.kbss.owlpersistence.ic.internalmodel;

import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDatatype;

public interface DatatypeParticipationConstraint extends
		ParticipationConstraintAnnotation {

	public OWLDataProperty getPredicate();

	public OWLDatatype getObject();

	public Integer getMinRequired();

	public Integer getMaxPossible();

}
