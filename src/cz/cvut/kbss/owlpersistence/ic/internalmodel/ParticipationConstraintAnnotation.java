package cz.cvut.kbss.owlpersistence.ic.internalmodel;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLProperty;

public interface ParticipationConstraintAnnotation extends
		IntegrityConstraintAnnotation {

	public OWLClass getSubject();

	public OWLProperty<?, ?> getPredicate();

	public OWLObject getObject();
}
