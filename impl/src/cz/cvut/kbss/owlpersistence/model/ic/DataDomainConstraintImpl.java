package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;

class DataDomainConstraintImpl extends AbstractDataDomainRangeConstraintImpl
		implements DataDomainConstraint {

	public DataDomainConstraintImpl(OWLDataProperty p, OWLClass o) {
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
