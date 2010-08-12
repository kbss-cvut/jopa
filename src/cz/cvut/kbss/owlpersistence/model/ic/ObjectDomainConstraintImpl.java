package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObjectProperty;

class ObjectDomainConstraintImpl extends
		AbstractObjectDomainRangeConstraintImpl implements
		ObjectDomainConstraint {

	public ObjectDomainConstraintImpl(OWLObjectProperty p, OWLClass o) {
		super(p, o);
	}

	@Override
	public OWLClass getDomain() {
		return getClazz();
	}

	@Override
	public void accept(IntegrityConstraintVisitor visitor) {
		visitor.visit(this);
	}
}
