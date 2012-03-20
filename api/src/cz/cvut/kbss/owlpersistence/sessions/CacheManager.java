package cz.cvut.kbss.owlpersistence.sessions;

import java.util.Map;

/**
 * This interface defines basic methods for accessing the shared live object
 * cache.
 * 
 * @author kidney
 * 
 */
public interface CacheManager {

	/**
	 * Add the specified object into the shared session cache. IRI will be
	 * extracted from the object.
	 * 
	 * @param object
	 *            The object to be added into the cache
	 */
	public void addObjectIntoCache(Object object);

	/**
	 * Add the specified object into the shared session cache. This method gets
	 * the IRI of the object in parameter, so it does not need to extract it
	 * itself.
	 * 
	 * @param object
	 *            The object to be added into the cache
	 * @param iri
	 *            IRI of the specified object
	 */
	public void addObjectIntoCache(Object object, Object iri);

	/**
	 * Add all of these objects into the shared session cache.
	 * 
	 * @param objects
	 */
	public void addObjectsIntoCache(Map<?, ?> objects);

	/**
	 * Returns true if the session cache contains such object.
	 * 
	 * @param object
	 * @return boolean
	 */
	public boolean containsObject(Object object);

	/**
	 * Returns true if the session cache contains an object with the specified
	 * IRI.
	 * 
	 * @param iri
	 * @return boolean
	 */
	public boolean containsObjectByIRI(Object iri);

	/**
	 * Try to find the specified object. It returns either the object or null.
	 * 
	 * @param domainObject
	 * @return The cached object or null
	 */
	public Object getObject(Object domainObject);

	/**
	 * Try to find object with the specified IRI.
	 * 
	 * @param iri
	 * @return Object with the specified IRI or null
	 */
	public Object getObjectByIRI(Object iri);

	/**
	 * Get the iri of the specified cached object.
	 * 
	 * @param object
	 * @return IRI
	 */
	public Object getIRIOfObject(Object object);

	/**
	 * Since we don't cache OWLIndividuals, this method is deprecated.
	 * 
	 * @param individual
	 * @return
	 */
	@Deprecated
	public Object getObjectByValue(Object individual);

	/**
	 * Release the cache.
	 */
	public void releaseCache();

	/**
	 * Remove objects with inferred attributes from the cache, since there are
	 * changes in the ontology that might influence the inferred attributes.
	 */
	public void clearInferredObjects();

	/**
	 * Remove the specified object from the cache.
	 * 
	 * @param object
	 *            Object to remove
	 */
	public void removeObjectFromCache(Object object);

	/**
	 * Remove the object with the specified IRI from the cache.
	 * 
	 * @param iri
	 */
	public void removeObjectFromCacheByIRI(Object iri);

}
