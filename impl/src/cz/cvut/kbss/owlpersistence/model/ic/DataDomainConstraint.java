package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;

public interface DataDomainConstraint extends IntegrityConstraint {

	public OWLClass getDomain();
	
	public OWLDataProperty getProperty();                   
}
