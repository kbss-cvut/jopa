package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDatatype;

class DataParticipationConstraintImpl extends
		AbstractParticipationConstraintImpl<OWLDataProperty, OWLDatatype>
		implements DataParticipationConstraint {

	public DataParticipationConstraintImpl(OWLClass subject,
			OWLDataProperty predicate, OWLDatatype object,
			int min, int max) {
		super(subject, predicate, object, min, max);
	}

	
	public void accept(IntegrityConstraintVisitor v) {
		v.visit(this);
	}
}
