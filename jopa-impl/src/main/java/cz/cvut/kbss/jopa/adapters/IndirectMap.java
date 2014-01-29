package cz.cvut.kbss.jopa.adapters;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class IndirectMap<K, V> extends IndirectCollection<Map<K, V>> implements
		Map<K, V> {

	private final Map<K, V> internalMap;

	/**
	 * No-arg constructor to support clone building
	 */
	IndirectMap() {
		this.internalMap = new HashMap<>();
	}

	public IndirectMap(Object owner, UnitOfWorkImpl persistenceContext,
			Map<K, V> map) {
		super(owner, persistenceContext);
		if (map == null) {
			throw new NullPointerException();
		}
		this.internalMap = map;
	}

	@Override
	public Map<K, V> getReferencedCollection() {
		throw new UnsupportedOperationException();
	}

	@Override
	public int size() {
		return internalMap.size();
	}

	@Override
	public boolean isEmpty() {
		return internalMap.isEmpty();
	}

	@Override
	public boolean containsKey(Object key) {
		return internalMap.containsKey(key);
	}

	@Override
	public boolean containsValue(Object value) {
		return internalMap.containsValue(value);
	}

	@Override
	public V get(Object key) {
		return internalMap.get(key);
	}

	@Override
	public V put(K key, V value) {
		V val = internalMap.put(key, value);
		persistChange();
		return val;
	}

	@Override
	public V remove(Object key) {
		V val = internalMap.remove(key);
		if (val != null) {
			persistChange();
		}
		return val;
	}

	@Override
	public void putAll(Map<? extends K, ? extends V> m) {
		internalMap.putAll(m);
		if (!m.isEmpty()) {
			persistChange();
		}
	}

	@Override
	public void clear() {
		if (!isEmpty()) {
			internalMap.clear();
			persistChange();
		}
	}

	@Override
	public Set<K> keySet() {
		return internalMap.keySet();
	}

	@Override
	public Collection<V> values() {
		return internalMap.values();
	}

	@Override
	public Set<java.util.Map.Entry<K, V>> entrySet() {
		return internalMap.entrySet();
	}
}
