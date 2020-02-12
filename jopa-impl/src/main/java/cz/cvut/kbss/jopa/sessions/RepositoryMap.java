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
package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

final class RepositoryMap {

	private static final URI DEFAULT_CONTEXT = URI.create("http://defaultContext");

	private final Map<URI, Map<Object, Object>> origsToClones = new HashMap<>();
	private Map<Object, Descriptor> entityDescriptors;

	void initDescriptors() {
		this.entityDescriptors = new IdentityHashMap<>();
	}

	void add(Descriptor descriptor, Object original, Object clone) {
		assert descriptor != null;
		assert original != null;
		// Null values are permitted

		final Map<Object, Object> entities = getMap(descriptor);
		entities.put(original, clone);
	}

	void remove(Descriptor descriptor, Object original) {
		assert descriptor != null;
		assert original != null;

		final Map<Object, Object> entities = getMap(descriptor);
		entities.remove(original);
	}

	/**
	 * Make sure to call {@link #initDescriptors()} before calling this.
	 */
	void addEntityToRepository(Object entity, Descriptor descriptor) {
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

	boolean contains(Descriptor descriptor, Object original) {
		assert descriptor != null;
		assert original != null;

		final Map<Object, Object> entities = getMap(descriptor);
		return entities.containsKey(original);
	}

	Object get(Descriptor descriptor, Object original) {
		assert descriptor != null;
		assert original != null;

		final Map<Object, Object> entities = getMap(descriptor);
		if (!entities.containsKey(original)) {
			return null;
		}
		return entities.get(original);
	}

	/**
	 * Make sure to call {@link #initDescriptors()} before calling this.
	 */
	Descriptor getEntityDescriptor(Object entity) {
		assert entityDescriptors != null;
		assert entity != null;

		return entityDescriptors.get(entity);
	}

	void clear() {
		origsToClones.values().forEach(Map::clear);
		if (entityDescriptors != null) {
			initDescriptors();
		}
	}

	private Map<Object, Object> getMap(Descriptor descriptor) {
		final URI ctx = descriptor.getContext() != null ? descriptor.getContext() : DEFAULT_CONTEXT;
		Map<Object, Object> entities;
		if (!origsToClones.containsKey(ctx)) {
			entities = new IdentityHashMap<>();
			origsToClones.put(ctx, entities);
		} else {
			entities = origsToClones.get(ctx);
		}
		return entities;
	}
}
