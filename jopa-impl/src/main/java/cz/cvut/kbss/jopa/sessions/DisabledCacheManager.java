package cz.cvut.kbss.jopa.sessions;

import java.util.Collections;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.jopa.sessions.CacheManager;

public class DisabledCacheManager implements CacheManager {

	protected AbstractSession session;

	public DisabledCacheManager(AbstractSession session) {
		this.session = session;
	}

	public void addObjectIntoCache(Object object) {
		// Nothing to do
	}

	public void add(Object iri, Object object) {
		// Nothing to do
	}

	public void addAll(Map<?, ?> objects) {
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

	public Object get(Class<?> cls, Object iri) {
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
		// Nothing to do
	}

	public boolean contains(Class<?> cls, Object primaryKey) {
		return false;
	}

	public void evict(Class<?> cls, Object primaryKey) {
		// Nothing to do

	}

	public void evict(Class<?> cls) {
		// Nothing to do

	}

	public void evictAll() {
		// Nothing to do

	}

	public boolean acquireReadLock() {
		return true;
	}

	public void releaseReadLock() {
		// Nothing to do
	}

	public boolean acquireWriteLock() {
		return true;
	}

	public void releaseWriteLock() {
		// Nothing to do
	}

	public void setInferredClasses(Set<Class<?>> inferredClasses) {
		// Nothing to do
	}
}
