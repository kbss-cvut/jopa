package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDatatype;

public interface DataRangeConstraint extends IntegrityConstraint {

	public OWLDatatype getRange();

	public OWLDataProperty getProperty();

	public OWLClass getOWLClass();
}
