package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLProperty;

public interface ParticipationConstraint<P extends OWLProperty<?, ?>, O extends OWLObject>
		extends IntegrityConstraint {

	public OWLClass getSubject();

	public P getPredicate();

	public O getObject();

	public int getMin();

	public int getMax();
}
