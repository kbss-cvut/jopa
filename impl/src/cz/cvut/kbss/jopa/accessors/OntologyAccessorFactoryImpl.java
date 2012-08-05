package cz.cvut.kbss.jopa.accessors;

import java.util.Map;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.Session;

public final class OntologyAccessorFactoryImpl implements
		OntologyAccessorFactory {

	private OntologyAccessor centralAccessor;

	public TransactionOntologyAccessor createOntologyAccessor(
			Map<String, String> properties, Metamodel metamodel, Session session) {
		return new OWLOntologyAccessor(properties, metamodel, session,
				centralAccessor);
		// More accessors can be added here
	}

	public OntologyAccessor createCentralAccessor(
			Map<String, String> properties, Metamodel metamodel, Session session) {
		if (centralAccessor == null) {
			centralAccessor = new OntologyAccessorImpl(properties, metamodel,
					session);
		}
		return centralAccessor;
	}

}
