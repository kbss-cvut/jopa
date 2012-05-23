package cz.cvut.kbss.owlpersistence.accessors;

import java.util.Map;

import cz.cvut.kbss.owlpersistence.model.metamodel.Metamodel;
import cz.cvut.kbss.owlpersistence.sessions.Session;

public interface OntologyAccessorFactory {

	/**
	 * Create an ontology accessor instance based on the specified properties
	 * (especially important are information about storage type) and initialize
	 * it with metamodel.
	 * 
	 * @param properties
	 *            Map of properties for the accessor.
	 * @param metamodel
	 *            Metamodel of entities.
	 * @param session
	 *            Parent session.
	 * @return Initialized accessor instance.
	 */
	public OntologyAccessor createOntologyAccessor(
			Map<String, String> properties, Metamodel metamodel, Session session);

}
