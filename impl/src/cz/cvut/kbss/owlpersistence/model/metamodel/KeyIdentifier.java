package cz.cvut.kbss.owlpersistence.model.metamodel;

import java.util.Set;

import cz.cvut.kbss.owlpersistence.model.IRI;

public interface KeyIdentifier extends Identifier {

	public Set<IRI> getOWLPropertyIRIs();

}
