package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;

import cz.cvut.kbss.jopa.model.EntityDescriptor;

final class RepositoryMap {

	private static final URI DEFAULT_CONTEXT = URI.create("http://defaultContext");

	private final Map<URI, Map<Object, Object>> map;
	private Map<Object, EntityDescriptor> entityDescriptors;

	RepositoryMap() {
		this.map = new HashMap<>();
	}

	void initDescriptors() {
		this.entityDescriptors = new IdentityHashMap<>();
	}

	void add(EntityDescriptor descriptor, Object key, Object value) {
		assert descriptor != null;
		assert key != null;
		// Null values are permitted

		final Map<Object, Object> entities = getMap(descriptor);
		entities.put(key, value);
	}

	void remove(EntityDescriptor descriptor, Object key) {
		assert descriptor != null;
		assert key != null;

		final Map<Object, Object> entities = getMap(descriptor);
		entities.remove(key);
	}

	/**
	 * Make sure to call {@link #initDescriptors()} before calling this.
	 */
	void addEntityToRepository(Object entity, EntityDescriptor descriptor) {
		assert entityDescriptors != null;
		entityDescriptors.put(entity, descriptor);
	}

	/**
	 * Make sure to call {@link #initDescriptors()} before calling this.
	 */
	void removeEntityToRepository(Object entity) {
		assert entityDescriptors != null;
		entityDescriptors.remove(entity);
	}

	boolean contains(EntityDescriptor descriptor, Object key) {
		assert descriptor != null;
		assert key != null;

		final Map<Object, Object> entities = getMap(descriptor);
		return entities.containsKey(key);
	}

	Object get(EntityDescriptor descriptor, Object key) {
		assert descriptor != null;
		assert key != null;

		final Map<Object, Object> entities = getMap(descriptor);
		if (!entities.containsKey(key)) {
			return null;
		}
		return entities.get(key);
	}

	/**
	 * Make sure to call {@link #initDescriptors()} before calling this.
	 */
	EntityDescriptor getEntityDescriptor(Object entity) {
		assert entityDescriptors != null;
		assert entity != null;

		return entityDescriptors.get(entity);
	}

	void clear() {
		for (Map<Object, Object> m : map.values()) {
			m.clear();
		}
		if (entityDescriptors != null) {
			initDescriptors();
		}
	}

	private Map<Object, Object> getMap(EntityDescriptor descriptor) {
		final URI ctx = descriptor.getEntityContext() != null ? descriptor.getEntityContext()
				: DEFAULT_CONTEXT;
		Map<Object, Object> entities;
		if (!map.containsKey(ctx)) {
			entities = new HashMap<>();
			map.put(ctx, entities);
		} else {
			entities = map.get(ctx);
		}
		return entities;
	}
}
