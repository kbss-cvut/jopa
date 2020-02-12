/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.adapters;

import java.lang.reflect.Field;
import java.util.*;

import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class IndirectMap<K, V> extends IndirectCollection<Map<K, V>> implements Map<K, V> {

    private final Map<K, V> internalMap;

    /**
     * No-arg constructor to support clone building
     */
    IndirectMap() {
        this.internalMap = new HashMap<>();
    }

    public IndirectMap(Object owner, Field f, UnitOfWorkImpl persistenceContext, Map<K, V> referencedMap) {
        super(owner, f, persistenceContext);
        this.internalMap = Objects.requireNonNull(referencedMap);
    }

    @Override
    public Map<K, V> getReferencedCollection() {
        return internalMap;
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

    @Override
    public boolean equals(Object o) {
        if (o instanceof Map) {
            if (o instanceof IndirectMap) {
                return internalMap.equals(((IndirectMap) o).internalMap);
            }
            return internalMap.equals(o);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return internalMap.hashCode();
    }

    @Override
    public String toString() {
        return internalMap.toString();
    }
}
