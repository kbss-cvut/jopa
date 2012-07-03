package cz.cvut.kbss.jopa.sessions;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.sessions.CacheManager;

/**
 * The CacheManager is responsible for managing the live object cache of our
 * session. It controls adding and removing objects from the cache. It supports
 * finding object by id or by a reference to it.
 * 
 * @author kidney
 * 
 */
public class CacheManagerImpl implements CacheManager {

	private Set<Class<?>> inferredClasses;

	protected Map<Object, Object> liveObjectCache;
	protected Map<Object, Object> inferredObjectCache;
	protected Map<Object, Object> nonInferredObjectCache;

	protected Map<Object, Object> objectToIRICache;
	protected Map<Object, Object> iriToObjectCache;

	protected AbstractSession session;

	public CacheManagerImpl(AbstractSession session) {
		this.session = session;
	}

	/**
	 * Adds the given object into the live object cache. If there is already an
	 * object with the same iri, this method does nothing and leaves the
	 * original object untouched.
	 */
	public void addObjectIntoCache(Object object) {
		if (object == null)
			return;
		Object iri = this.session.getOntologyAccessor().getIdentifier(object);
		this.addObjectIntoCache(object, iri);
	}

	public void addObjectIntoCache(Object object, Object iri) {
		if (object == null || iri == null) {
			return;
		}
		if (getLiveObjectCache().containsKey(object)
				|| getIRIToObjectCache().containsKey(iri)) {
			throw new OWLPersistenceException(
					"Trying to add an object with existing IRI.");
		}
		this.putObjectIntoCache(object, iri);
	}

	/**
	 * Put the specified object to all types of cache we use.
	 * 
	 * @param object
	 *            The entity to cache.
	 * @param iri
	 *            The IRI of the cached entity.
	 */
	protected void putObjectIntoCache(Object object, Object iri) {
		if (!getInferredClasses().contains(object.getClass())) {
			getNonInferredObjectCache().put(object, object);
		} else {
			getInferredObjectCache().put(object, object);
		}
		getLiveObjectCache().put(object, object);
		getIRIToObjectCache().put(iri, object);
		getObjectToIRICache().put(object, iri);
	}

	/**
	 * Remove the specified object from live object cache and from the inferred
	 * or non-inferred cache according to its class.
	 * 
	 * @param object
	 *            The object to remove.
	 */
	protected void removeObject(Object object) {
		getLiveObjectCache().remove(object);
		getInferredObjectCache().remove(object);
		getNonInferredObjectCache().remove(object);
	}

	/**
	 * This method adds the whole map of objects into the liveObjectCache. It
	 * expects the map to contain pairs of Object's iri and the Object itself.
	 * 
	 * @param objects
	 *            Map
	 */
	public void addObjectsIntoCache(Map<?, ?> objects) {
		if (objects == null) {
			return;
		}
		for (Object entity : objects.values()) {
			addObjectIntoCache(entity);
		}
	}

	/**
	 * Create a particular implementation of the Map interface. This method
	 * enables the implementation to be changed according to our needs.
	 * 
	 * @return A new map instance.
	 */
	protected Map<Object, Object> createMap() {
		return new HashMap<Object, Object>();
	}

	/**
	 * INTERNAL method.
	 * 
	 * @return Map
	 */
	public Map<Object, Object> getLiveObjectCache() {
		if (this.liveObjectCache == null) {
			this.liveObjectCache = createMap();
		}
		return this.liveObjectCache;
	}

	protected Map<Object, Object> getNonInferredObjectCache() {
		if (this.nonInferredObjectCache == null) {
			this.nonInferredObjectCache = createMap();
		}
		return this.nonInferredObjectCache;
	}

	protected Map<Object, Object> getInferredObjectCache() {
		if (this.inferredObjectCache == null) {
			this.inferredObjectCache = createMap();
		}
		return this.inferredObjectCache;
	}

	protected Map<Object, Object> getObjectToIRICache() {
		if (this.objectToIRICache == null) {
			this.objectToIRICache = createMap();
		}
		return this.objectToIRICache;
	}

	protected Map<Object, Object> getIRIToObjectCache() {
		if (this.iriToObjectCache == null) {
			this.iriToObjectCache = createMap();
		}
		return this.iriToObjectCache;
	}

	public boolean containsObject(Object entity) {
		return getLiveObjectCache().containsKey(entity);
	}

	/**
	 * Looks up the object. If such object is not found, this method returns
	 * null. Usually lookup by IRI should be used.
	 * 
	 * @param domainObject
	 */
	public Object getObject(Object domainObject) {
		if (domainObject == null) {
			return null;
		}
		Object result = getLiveObjectCache().get(domainObject);
		return result;
	}

	/**
	 * This method tries to find the given object by its value. Since it has to
	 * go through the whole cache, it may be rather slow. Instead of this
	 * method, clients should use the getObjectById method.
	 * 
	 * @see #getObject(Object)
	 * 
	 * @param domainObject
	 *            The object we are looking for
	 */
	@Deprecated
	public Object getObjectByValue(Object domainObject) {
		if (domainObject == null) {
			return null;
		}
		if (getLiveObjectCache().containsValue(domainObject)) {
			for (Object o : getLiveObjectCache().keySet()) {
				if (o.equals(domainObject)) {
					return o;
				}
			}
		}
		return null;
	}

	/**
	 * Releases the live object cache.
	 */
	public void releaseCache() {
		this.liveObjectCache = null;
		this.iriToObjectCache = null;
		this.objectToIRICache = null;
		this.inferredObjectCache = null;
		this.nonInferredObjectCache = null;
	}
	
	public void clearInferredObjects() {
		Object iri = null;
		for (Object ob : getInferredObjectCache().keySet()) {
			getLiveObjectCache().remove(ob);
			iri = getObjectToIRICache().remove(ob);
			getIRIToObjectCache().remove(iri);
		}
		getInferredObjectCache().clear();
	}

	/**
	 * Removes the specified object from the live object cache.
	 * 
	 * @param object
	 */
	public void removeObjectFromCache(Object object) {
		if (object == null) {
			return;
		}
		Object iri = getObjectToIRICache().remove(object);
		if (iri != null) {
			getIRIToObjectCache().remove(iri);
		}
		removeObject(object);
	}

	public boolean containsObjectByIRI(Object iri) {
		if (iri == null) {
			return false;
		}
		return getIRIToObjectCache().containsKey(iri);
	}

	public Object getObjectByIRI(Object iri) {
		if (iri == null) {
			return null;
		}
		return getIRIToObjectCache().get(iri);
	}

	public Object getIRIOfObject(Object object) {
		if (object == null) {
			return null;
		}
		return getObjectToIRICache().get(object);
	}

	public void removeObjectFromCacheByIRI(Object iri) {
		if (iri == null) {
			return;
		}
		Object object = getIRIToObjectCache().remove(iri);
		if (object != null) {
			getObjectToIRICache().remove(object);
			removeObject(object);
		}
	}

	public Set<Class<?>> getInferredClasses() {
		if (inferredClasses == null) {
			this.inferredClasses = new HashSet<Class<?>>();
		}
		return inferredClasses;
	}

	public void setInferredClasses(Set<Class<?>> inferredClasses) {
		this.inferredClasses = inferredClasses;
	}

}
