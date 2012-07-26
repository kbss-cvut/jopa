package cz.cvut.kbss.jopa.accessors;

import java.util.Map;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.Session;

public final class OntologyAccessorFactoryImpl implements
		OntologyAccessorFactory {

	public OntologyAccessor createOntologyAccessor(
			Map<String, String> properties, Metamodel metamodel, Session session) {
		return new OWLOntologyAccessor(properties, metamodel, session);
		// More accessors can be added here
	}

}
