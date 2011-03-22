/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

package cz.cvut.kbss.owlpersistence.owlapi;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.owlpersistence.model.EntityManager;
import cz.cvut.kbss.owlpersistence.model.EntityManagerFactory;
import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.model.PersistenceUnitUtil;
import cz.cvut.kbss.owlpersistence.model.metamodel.Metamodel;

public class EntityManagerFactoryImpl implements EntityManagerFactory,
		PersistenceUnitUtil {

	private boolean open = true;

	private final Set<AbstractEntityManager> em = new HashSet<AbstractEntityManager>();
	private final Map<String, String> properties;

	private MetamodelImpl metamodel = null;

	public EntityManagerFactoryImpl(final Map<String, String> properties) {
		this.properties = properties;
	}

	public void close() {
		open = false;

		for (final EntityManager m : em) {
			// TODO try-catch
			m.close();
		}
	}

	public EntityManager createEntityManager() {
		return this
				.createEntityManager(Collections.<String, String> emptyMap());
	}

	public EntityManager createEntityManager(Map<String, String> map) {
		if (!open) {
			throw new IllegalStateException(
					"The OWLEntityManager has been closed.");
		}

		final Map<String, String> newMap = new HashMap<String, String>(map);

		newMap.putAll(properties);
		newMap.putAll(map);

		final AbstractEntityManager c = new EntityManagerImpl(this, newMap);

		em.add(c);
		return c;
	}

	public boolean isOpen() {
		return open;
	}

	public Map<String, String> getProperties() {
		return properties;
	}

	public Set<AbstractEntityManager> getEntityManagers() {
		return Collections.unmodifiableSet(em);
	}

	public Metamodel getMetamodel() {
		if (metamodel == null) {
			metamodel = new MetamodelImpl(this);
		}

		return metamodel;
	}

	public PersistenceUnitUtil getPersistenceUnitUtil() {
		return this;
	}

	public Object getIdentifier(Object entity) {
		try {
			return getMetamodel().entity(entity.getClass()).getIdentifier()
					.getJavaField().get(entity);
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException();
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException();
		}
	}

	public boolean isLoaded(Object entity, String attributeName) {
		for (final AbstractEntityManager emi : em) {
			if (emi.contains(entity)) {
				if (attributeName == null) {
					return true;
				}

				return emi.isLoaded(entity, attributeName);
			}
		}

		return false;
	}

	public boolean isLoaded(Object entity) {
		return isLoaded(entity);
	}

}
