package cz.cvut.kbss.owlpersistence.ic.internalmodel;

import org.semanticweb.owlapi.model.OWLClass;

public interface SubClassOfSpecification extends OWLPersistenceAnnotation {

	public OWLClass getSubClass();

	public OWLClass getSuperClass();
}
