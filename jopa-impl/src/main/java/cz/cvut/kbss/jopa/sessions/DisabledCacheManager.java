package cz.cvut.kbss.jopa.sessions;

import java.util.Set;

import cz.cvut.kbss.jopa.model.EntityDescriptor;

public class DisabledCacheManager implements CacheManager {

	@Override
	public boolean contains(Class<?> cls, Object primaryKey) {
		return false;
	}

	@Override
	public boolean contains(EntityDescriptor repository, Class<?> cls, Object primaryKey) {
		return false;
	}

	@Override
	public void evict(EntityDescriptor repository, Class<?> cls, Object primaryKey) {
		// Do nothing
	}

	@Override
	public void evict(Class<?> cls) {
		// Do nothing
	}

	@Override
	public void evict(EntityDescriptor repository) {
		// Do nothing
	}

	@Override
	public void evictAll() {
		// Do nothing
	}

	@Override
	public void add(EntityDescriptor repository, Object primaryKey, Object entity) {
		// Do nothing
	}

	@Override
	public <T> T get(EntityDescriptor repository, Class<T> cls, Object primaryKey) {
		return null;
	}

	@Override
	public void clearInferredObjects() {
		// Do nothing
	}

	@Override
	public boolean acquireReadLock() {
		return true;
	}

	@Override
	public void releaseReadLock() {
		// Do nothing
	}

	@Override
	public boolean acquireWriteLock() {
		return true;
	}

	@Override
	public void releaseWriteLock() {
		// Do nothing
	}

	@Override
	public void setInferredClasses(Set<Class<?>> inferredClasses) {
		// Do nothing
	}
}
