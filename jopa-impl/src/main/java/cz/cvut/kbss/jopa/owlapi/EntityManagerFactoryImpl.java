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

package cz.cvut.kbss.jopa.owlapi;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.PersistenceUnitUtil;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.Cache;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

public class EntityManagerFactoryImpl implements EntityManagerFactory,
		PersistenceUnitUtil {

	private boolean open = true;

	private final Set<AbstractEntityManager> em = new HashSet<AbstractEntityManager>();
	private final Map<String, String> properties;
	private final List<OntologyStorageProperties> storageProperties;

	private ServerSession serverSession;

	private MetamodelImpl metamodel = null;

	public EntityManagerFactoryImpl(final Map<String, String> properties) {
		this.properties = properties;
		// TODO The storage properties should be read from persistence.xml
		this.storageProperties = Collections.emptyList();
	}

	public EntityManagerFactoryImpl(
			List<OntologyStorageProperties> storageProperties,
			Map<String, String> properties) {
		if (storageProperties == null) {
			throw new NullPointerException();
		}
		this.properties = properties;
		this.storageProperties = storageProperties;
	}

	public void close() {
		open = false;

		for (final EntityManager m : em) {
			if (m.isOpen()) {
				m.close();
			}
		}
		serverSession.close();
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

		initServerSession(newMap);

		final AbstractEntityManager c = new EntityManagerImpl(this, newMap,
				this.serverSession);

		em.add(c);
		return c;
	}

	/**
	 * Initializes the server session if necessary.
	 * 
	 * @param newMap
	 *            Map of properties. These properties specify primarily the
	 *            connection to the underlying ontology.
	 */
	private void initServerSession(Map<String, String> newMap) {
		if (this.serverSession == null) {
			this.serverSession = new ServerSession(storageProperties, newMap,
					getMetamodel());
		}
	}

	/**
	 * The server session should by initialized by now, but to make sure, there
	 * is default initialization with an empty properties map.
	 * 
	 * @return The ServerSession for this factory.
	 */
	public ServerSession getServerSession() {
		if (this.serverSession == null) {
			this.initServerSession(Collections.<String, String> emptyMap());
		}
		return this.serverSession;
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

	public Cache getCache() {
		if (!isOpen()) {
			throw new IllegalStateException(
					"The entity manager factory is closed.");
		}
		return getServerSession().getLiveObjectCache();
	}

}
