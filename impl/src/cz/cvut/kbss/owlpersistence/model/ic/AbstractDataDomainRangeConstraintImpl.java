package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;

abstract class AbstractDataDomainRangeConstraintImpl implements
		IntegrityConstraint {

	private final OWLClass o;
	private final OWLDataProperty p;

	public AbstractDataDomainRangeConstraintImpl(OWLDataProperty p, OWLClass o) {
		this.o = o;
		this.p = p;
	}

	public OWLClass getClazz() {
		return o;
	}

	public OWLDataProperty getProperty() {
		return p;
	}
}
