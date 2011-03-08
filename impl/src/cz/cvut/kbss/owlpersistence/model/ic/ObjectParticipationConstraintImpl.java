package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObjectProperty;

class ObjectParticipationConstraintImpl extends
		AbstractParticipationConstraintImpl<OWLObjectProperty, OWLClass>
		implements ObjectParticipationConstraint {

	public ObjectParticipationConstraintImpl(OWLClass subject,
			OWLObjectProperty predicate, OWLClass object,
			int min, int max) {
		super(subject, predicate, object, min, max);
	}

	@Override
	public void accept(IntegrityConstraintVisitor v) {
		v.visit(this);
	}
}
