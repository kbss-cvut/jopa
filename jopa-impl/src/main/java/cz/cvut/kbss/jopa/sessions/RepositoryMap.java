package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.jopa.model.RepositoryID;

final class RepositoryMap {

	private final List<Map<URI, Map<Object, Object>>> map;
	private Map<Object, RepositoryID> entityToRepository;

	public RepositoryMap(int size) {
		this.map = new ArrayList<>(size);
		for (int i = 0; i < size; i++) {
			map.add(new HashMap<URI, Map<Object, Object>>());
		}
	}

	void initEntityToRepository() {
		this.entityToRepository = new IdentityHashMap<>();
	}

	void add(RepositoryID repository, Object key, Object value) {
		assert repository != null;
		assert !repository.getContexts().isEmpty();
		assert repository.getRepository() < map.size();
		assert key != null;
		// Null values are permitted

		final Map<Object, Object> entities = getMap(repository);
		entities.put(key, value);
	}

	void remove(RepositoryID repository, Object key) {
		assert repository != null;
		assert !repository.getContexts().isEmpty();
		assert repository.getRepository() < map.size();
		assert key != null;

		final Map<Object, Object> entities = getMap(repository);
		entities.remove(key);
	}

	void addEntityToRepository(Object entity, RepositoryID repository) {
		assert entityToRepository != null;
		entityToRepository.put(entity, repository);
	}

	void removeEntityToRepository(Object entity) {
		assert entityToRepository != null;
		entityToRepository.remove(entity);
	}

	boolean contains(RepositoryID repository, Object key) {
		assert repository != null;
		assert !repository.getContexts().isEmpty();
		assert repository.getRepository() < map.size();
		assert key != null;

		final Map<Object, Object> entities = getMap(repository);
		return entities.containsKey(key);
	}

	Object get(RepositoryID repository, Object key) {
		assert repository != null;
		assert !repository.getContexts().isEmpty();
		assert repository.getRepository() < map.size();
		assert key != null;

		final Map<Object, Object> entities = getMap(repository);
		if (!entities.containsKey(key)) {
			return null;
		}
		return entities.get(key);
	}

	RepositoryID getRepositoryID(Object entity) {
		assert entityToRepository != null;
		assert entity != null;

		return entityToRepository.get(entity);
	}

	void clear() {
		for (Map<URI, Map<Object, Object>> m : map) {
			m.clear();
		}
		if (entityToRepository != null) {
			initEntityToRepository();
		}
	}

	private Map<Object, Object> getMap(RepositoryID repository) {
		final Map<URI, Map<Object, Object>> m = map.get(repository.getRepository());
		final URI ctx = repository.getContexts().iterator().next();
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
