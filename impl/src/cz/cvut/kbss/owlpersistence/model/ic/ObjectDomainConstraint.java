package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObjectProperty;

public interface ObjectDomainConstraint extends IntegrityConstraint {

	public OWLClass getDomain();
	
	public OWLObjectProperty getProperty();                   
}
