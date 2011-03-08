package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDatatype;

class DataRangeConstraintImpl implements
		DataRangeConstraint {

	private final OWLClass subj;
	private final OWLDataProperty p;
	private final OWLDatatype range;

	public DataRangeConstraintImpl(OWLClass subj, OWLDataProperty p, OWLDatatype range) {
		this.subj = subj;
		this.p = p;
		this.range = range;
	}

	public OWLDataProperty getProperty() {
		return p;
	}
	
	@Override
	public OWLDatatype getRange() {
		return range;
	}

	@Override
	public void accept(IntegrityConstraintVisitor visitor) {
		visitor.visit((DataRangeConstraint) this);
	}

	@Override
	public OWLClass getOWLClass() {
		return subj;
	}
}
