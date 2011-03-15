package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObjectProperty;

class ObjectDomainConstraintImpl extends
		AbstractObjectDomainRangeConstraintImpl implements
		ObjectDomainConstraint {

	public ObjectDomainConstraintImpl(OWLObjectProperty p, OWLClass o) {
		super(p, o);
	}

	
	public OWLClass getDomain() {
		return getClazz();
	}

	
	public void accept(IntegrityConstraintVisitor visitor) {
		visitor.visit(this);
	}
}
