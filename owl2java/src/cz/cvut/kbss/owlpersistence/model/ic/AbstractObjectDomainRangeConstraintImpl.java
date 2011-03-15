package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObjectProperty;

abstract class AbstractObjectDomainRangeConstraintImpl implements
		IntegrityConstraint {

	private final OWLClass o;
	private final OWLObjectProperty p;

	public AbstractObjectDomainRangeConstraintImpl(OWLObjectProperty p,
			OWLClass o) {
		this.o = o;
		this.p = p;
	}

	public OWLClass getClazz() {
		return o;
	}

	public OWLObjectProperty getProperty() {
		return p;
	}
}
