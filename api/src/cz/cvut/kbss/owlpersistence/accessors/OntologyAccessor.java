package cz.cvut.kbss.owlpersistence.accessors;

import java.util.List;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.owlpersistence.model.EntityManager;
import cz.cvut.kbss.owlpersistence.model.query.Query;
import cz.cvut.kbss.owlpersistence.model.query.TypedQuery;
import cz.cvut.kbss.owlpersistence.sessions.UnitOfWork;

/**
 * This interface defines basic methods for accessing, retrieving and writing
 * data into ontology.
 * 
 * @author kidney
 * 
 */
public interface OntologyAccessor {

	/**
	 * Generate and set a new IRI for the specified entity.
	 * 
	 * @param entity
	 *            The entity IRI will be generated for.
	 */
	public void generateNewIRI(final Object entity);

	/**
	 * Persist the given entity. This method is used only for persisting new
	 * entities.
	 * 
	 * @param entity
	 *            The entity to persist.
	 * @param uow
	 *            The UnitOfWork which initiated the persist.
	 */
	public void persistEntity(Object entity, UnitOfWork uow);

	/**
	 * Persists the specified existing entity. This means persisting the new
	 * state of an existing entity.
	 * 
	 * @param entity
	 *            The entity to save.
	 * @param uow
	 *            The UnitOfWork which initiated the persist.
	 */
	public void persistExistingEntity(Object entity, UnitOfWork uow);

	/**
	 * Remove the given entity from the ontology.
	 * 
	 * @param entity
	 */
	public void removeEntity(Object entity);

	/**
	 * Load and reconstruct object with the given uri from the ontology.
	 * 
	 * @param uri
	 *            Object
	 * @return The object with specified uri or null
	 */
	public <T> T readEntity(Class<T> cls, Object uri);

	/**
	 * Create an instance of Query for executing a Java Persistence query
	 * language statement.
	 * 
	 * @param qlString
	 *            a Java Persistence query string
	 * @param em
	 *            The calling EntityManager instance.
	 * @return the new query instance
	 * @throws IllegalArgumentException
	 *             if query string is not valid
	 */
	public Query<?> createQuery(String qlString, final EntityManager em);

	/**
	 * Create an instance of Query for executing typed Java Persistence query
	 * language statement.
	 * 
	 * @param query
	 *            The query to execute.
	 * @param resultClass
	 *            Expected result class.
	 * @param sparql
	 *            True if the query is a Sparql query.
	 * @param em
	 *            The calling EntityManager instance.
	 * @return
	 */
	public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass,
			boolean sparql, final EntityManager em);

	/**
	 * Create an instance of Query for executing a native SPARQL-DL query in
	 * SPARQL syntax.
	 * 
	 * @param sparql
	 *            a native SQL query string
	 * @param em
	 *            The calling EntityManager instance.
	 * @return the new query instance
	 */
	public Query<List<String>> createNativeQuery(String sparql,
			final EntityManager em);

	/**
	 * Write the given OWLOntologyChange list. This method should not be used,
	 * entity operations should be used instead.
	 * 
	 * @param changes
	 */
	public void writeChanges(List<OWLOntologyChange> changes);

	/**
	 * Write the given OWLOntologyChange. This method should not be used, entity
	 * operations should be used instead.
	 * 
	 * @param change
	 */
	public void writeChange(OWLOntologyChange change);

	/**
	 * Save the state of the working ontology.
	 */
	public void saveWorkingOntology();

	/**
	 * Is an entity with the given IRI in the ontology signature?
	 * 
	 * @param uri
	 *            IRI
	 * @param searchImports
	 *            This parameter specifies if the accessor should also search
	 *            included ontologies.
	 * @return
	 */
	public boolean isInOntologySignature(IRI uri, boolean searchImports);

	/**
	 * Get the OWLNamedIndividual with the specified IRI.
	 * 
	 * @param identifier
	 * @return OWLNamedIndividual
	 */
	public OWLNamedIndividual getOWLNamedIndividual(IRI identifier);

	/**
	 * Get the IRI identifier of the specified object.
	 * 
	 * @param object
	 *            Object
	 * @return IRI of the given object
	 */
	public IRI getIdentifier(Object object);

	/**
	 * Write all pending changes and close the ontology connection.
	 */
	public void close();

}
