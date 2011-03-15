package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObjectProperty;

class ObjectRangeConstraintImpl extends
		AbstractObjectDomainRangeConstraintImpl implements
		ObjectRangeConstraint {

	final OWLClass subj;
	
	public ObjectRangeConstraintImpl(OWLClass c,OWLObjectProperty p, OWLClass o) {
		super(p, o);
		this.subj = c;
	}

	
	public OWLClass getRange() {
		return getClazz();
	}

	
	public void accept(IntegrityConstraintVisitor visitor) {
		visitor.visit(this);
	}

	
	public OWLClass getOWLClass() {
		return subj;
	}
}
