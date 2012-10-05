package cz.cvut.kbss.jopa.accessors;

import java.util.Map;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.Session;

public final class OntologyAccessorFactoryImpl implements
		OntologyAccessorFactory {

	private OntologyAccessor centralAccessor;

	public OntologyAccessor createCentralAccessor(
			Map<String, String> properties, Metamodel metamodel, Session session) {
		if (centralAccessor == null) {
			centralAccessor = new OntologyAccessorImpl(properties, metamodel,
					session);
		}
		return centralAccessor;
	}

	public TransactionOntologyAccessor createTransactionalAccessor(
			OntologyDataHolder dataHolder, Session session) {
		if (centralAccessor == null) {
			throw new IllegalStateException(
					"Cannot create transactional accessor, because the central accessor is not initialized.");
		}
		return new OWLOntologyAccessor(session, centralAccessor, dataHolder);
	}

}
