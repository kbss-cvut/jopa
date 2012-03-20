package cz.cvut.kbss.owlpersistence.sessions;

import java.util.Collections;
import java.util.Map;

public class DisabledCacheManager implements CacheManager {

	protected AbstractSession session;

	public DisabledCacheManager(AbstractSession session) {
		this.session = session;
	}

	public void addObjectIntoCache(Object object) {
		// Nothing to do
	}

	public void addObjectIntoCache(Object object, Object iri) {
		// Nothing to do
	}

	public void addObjectsIntoCache(Map<?, ?> objects) {
		// Nothing to do
	}

	public boolean containsObject(Object object) {
		return false;
	}

	public Map<Object, Object> getLiveObjectCache() {
		return Collections.emptyMap();
	}

	public boolean containsObjectByIRI(Object iri) {
		return false;
	}

	public Object getObject(Object domainObject) {
		return null;
	}

	public Object getObjectByIRI(Object iri) {
		return null;
	}

	public Object getIRIOfObject(Object object) {
		return null;
	}

	public Object getObjectByValue(Object individual) {
		return null;
	}

	public void releaseCache() {
		// Nothing to do
	}

	public void clearInferredObjects() {
		// Nothing to do
	}

	public void removeObjectFromCache(Object object) {
		// Nothing to do
	}

	public void removeObjectFromCacheByIRI(Object iri) {
		// Nothing to ddo
	}

}
