package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObjectProperty;

public interface ObjectRangeConstraint extends IntegrityConstraint {

	public OWLClass getRange();
	
	public OWLObjectProperty getProperty();                   

	public OWLClass getOWLClass();
}
