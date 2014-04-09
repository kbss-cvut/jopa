package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.model.Repository;

final class RepositoryMap {

	private final Map<Integer, Map<URI, Map<Object, Object>>> map;
	private Map<Object, EntityDescriptor> entityDescriptors;

	public RepositoryMap(List<Repository> repos) {
		final int size = repos.size();
		this.map = new HashMap<>(size);
		for (Repository r : repos) {
			map.put(r.getId(), new HashMap<URI, Map<Object, Object>>());
		}
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
		assert descriptor.getRepository() < map.size();
		assert key != null;

		final Map<Object, Object> entities = getMap(descriptor);
		entities.remove(key);
	}

	void addEntityToRepository(Object entity, EntityDescriptor descriptor) {
		assert entityDescriptors != null;
		entityDescriptors.put(entity, descriptor);
	}

	void removeEntityToRepository(Object entity) {
		assert entityDescriptors != null;
		entityDescriptors.remove(entity);
	}

	boolean contains(EntityDescriptor descriptor, Object key) {
		assert descriptor != null;
		assert descriptor.getRepository() < map.size();
		assert key != null;

		final Map<Object, Object> entities = getMap(descriptor);
		return entities.containsKey(key);
	}

	Object get(EntityDescriptor descriptor, Object key) {
		assert descriptor != null;
		assert descriptor.getRepository() < map.size();
		assert key != null;

		final Map<Object, Object> entities = getMap(descriptor);
		if (!entities.containsKey(key)) {
			return null;
		}
		return entities.get(key);
	}

	EntityDescriptor getEntityDescriptor(Object entity) {
		assert entityDescriptors != null;
		assert entity != null;

		return entityDescriptors.get(entity);
	}

	void clear() {
		for (Map<URI, Map<Object, Object>> m : map.values()) {
			m.clear();
		}
		if (entityDescriptors != null) {
			initDescriptors();
		}
	}

	private Map<Object, Object> getMap(EntityDescriptor descriptor) {
		final Map<URI, Map<Object, Object>> m = map.get(descriptor.getRepository());
		if (m == null) {
			throw new IllegalArgumentException("Unknown repository " + descriptor);
		}
		final URI ctx = descriptor.getEntityContext();
		Map<Object, Object> entities;
		if (!m.containsKey(ctx)) {
			entities = new HashMap<>();
			m.put(ctx, entities);
		} else {
			entities = m.get(ctx);
		}
		return entities;
	}
}
