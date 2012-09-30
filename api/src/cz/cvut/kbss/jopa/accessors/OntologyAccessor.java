package cz.cvut.kbss.jopa.accessors;

import java.util.List;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;

/**
 * This interface defines the main access point to the ontology.
 * 
 * This accessor is shared by all existing entity manager sessions. Thus, all
 * operations run on objects of this interface have to be preceded by acquiring
 * a corresponding lock (read or write) and succeeded by releasing this lock.
 * Failure to do so can result in undefined behavior.
 * 
 * @author kidney
 * 
 */
public interface OntologyAccessor {

	/**
	 * Acquire a read lock. </p>
	 * 
	 * Read locks are non-exclusive, i. e. multiple threads can simultaneously
	 * acquire read locks and read the same data. If an exclusive lock is held
	 * (see {@link #acquireWriteLock()}) the thread blocks until the exclusive
	 * lock is released.
	 * 
	 * @return True if the lock was successfully acquired, false otherwise.
	 */
	public boolean acquireReadLock();

	/**
	 * Release the previously acquired read lock. </p>
	 * 
	 * See {@link #acquireReadLock()}.
	 */
	public void releaseReadLock();

	/**
	 * Acquire an exclusive write lock. </p>
	 * 
	 * Acquiring write lock exclusively locks the accessor so the calling thread
	 * can modify the data in the ontology. Since the write lock is exclusive,
	 * if another thread already holds a lock (no matter whether exclusive or
	 * non-exclusive), the calling thread waits until the lock is released.
	 * 
	 * @return True if the lock was successfully acquired, false otherwise.
	 */
	public boolean acquireWriteLock();

	/**
	 * Release the previously acquired write lock. </p>
	 * 
	 * See {@link #acquireWriteLock()}
	 */
	public void releaseWriteLock();

	/**
	 * Create and get a clone of the working ontology structures. </p>
	 * 
	 * This operation requires only read lock, since it does not modify existing
	 * resources.
	 * 
	 * @return Clone of the working ontology structures.
	 */
	public OntologyDataHolder cloneOntologyStructures();

	/**
	 * Write changes to the underlying ontology. </p>
	 * 
	 * This method applies the specified changes to the underlying working
	 * ontology and saves the working ontology. This operation effectively
	 * propagates the changes to the ontology storage and makes the persistent.
	 * 
	 * @param changes
	 *            List of changes to apply
	 * @throws OWLPersistenceException
	 *             If any exception occurs while writing the changes or
	 *             persisting the ontology, it is wrapped in an
	 *             OWLPersistenceException object and thrown out.
	 */
	public void writeChanges(final List<OWLOntologyChange> changes)
			throws OWLPersistenceException;

	/**
	 * Load and reconstruct object with the given URI from the ontology. </p>
	 * 
	 * @param cls
	 *            Entity class to which the returned object should be cast
	 * @param uri
	 *            URI of the entity to search for
	 * @return The object with specified URI or null.
	 */
	public <T> T readEntity(Class<T> cls, Object uri);

	/**
	 * Explicitly save the working ontology. </p>
	 * 
	 * Note that saving the working ontology is usually not necessary, because
	 * the only operation allowed to modify the working ontology (see
	 * {@link #writeChanges(List)}) should save the working ontology explicitly.
	 * This method is here just to make the API more flexible.
	 * 
	 * @throws OWLPersistenceException
	 *             If any exception occurs while saving the working ontology.
	 */
	public void saveWorkingOntology() throws OWLPersistenceException;

	/**
	 * Close the connection to the working ontology. </p>
	 * 
	 * @throws OWLPersistenceException
	 *             If an exception is thrown while closing the connection to the
	 *             ontology.
	 */
	public void close() throws OWLPersistenceException;

	/**
	 * Indicates whether the accessor is open. </p>
	 * 
	 * Returns true until the accessor has been closed.
	 * 
	 * @return Boolean indicating whether the accessor is open
	 */
	public boolean isOpen();

	/**
	 * Generate new identifier for the specified entity. </p>
	 * 
	 * If the specified object is not entity or is already present in the
	 * working ontology, null is returned.
	 * 
	 * @param entity
	 *            The entity for which identifier should be generated
	 * @return The new generated IRI.
	 */
	public IRI generateNewIdentifier(Object entity);

	/**
	 * Check if entity with the specified IRI is in the working ontology. </p>
	 * 
	 * If the specified identifier is null, false is returned.
	 * 
	 * @param identifier
	 *            IRI of the entity to look for
	 * @param shouldSearchImports
	 *            This parameter specifies if the accessor should also search
	 *            included ontologies.
	 * @return True if an individual with the specified IRI is in the ontology
	 *         signature, false otherwise.
	 */
	public boolean isInOntologySignature(IRI identifier,
			boolean shouldSearchImports);

	/**
	 * Get the OWLNamedIndividual with the specified IRI. </p>
	 * 
	 * @param identifier
	 *            Identifier of the individual to search for
	 * @return OWLNamedIndividual or null, if nothing is found
	 */
	public OWLNamedIndividual getOWLNamedIndividual(IRI identifier);

	/**
	 * Check if the integrity constraints are not violated. </p>
	 * 
	 * 
	 * @param entity
	 *            The entity to check IC's for
	 * @param id
	 *            Identifier of the entity
	 * @param attribute
	 *            Attribute constraints will be checked for
	 * @throws IntegrityConstraintsViolatedException
	 *             If the integrity constraints are violated.
	 */
	public void checkIntegrityConstraints(Object entity, IRI id,
			Attribute<?, ?> attribute);
}
