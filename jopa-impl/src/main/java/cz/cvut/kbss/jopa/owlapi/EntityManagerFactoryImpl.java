/**
 * Copyright (C) 2011 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */

package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.loaders.EntityLoader;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.PersistenceUnitUtil;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.Cache;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class EntityManagerFactoryImpl implements EntityManagerFactory, PersistenceUnitUtil {

    private volatile boolean open = true;

    private final Set<AbstractEntityManager> em;
    private final Configuration configuration;
    private final OntologyStorageProperties storageProperties;

    private volatile ServerSession serverSession;

    private volatile MetamodelImpl metamodel;

    public EntityManagerFactoryImpl(final Map<String, String> properties) {
        this.em = Collections
                .newSetFromMap(new ConcurrentHashMap<>());
        this.configuration = new Configuration(properties != null ? properties : Collections.emptyMap());
        // TODO The storage properties should be read from persistence.xml
        this.storageProperties = null;
        initMetamodel();
    }

    public EntityManagerFactoryImpl(OntologyStorageProperties storageProperties, Map<String, String> properties) {
        if (storageProperties == null) {
            throw new NullPointerException();
        }
        this.em = Collections.newSetFromMap(new ConcurrentHashMap<>());
        this.configuration = new Configuration(properties != null ? properties : Collections.emptyMap());
        this.storageProperties = storageProperties;
        initMetamodel();
    }

    private void initMetamodel() {
        this.metamodel = new MetamodelImpl(configuration, new EntityLoader());
    }

    public void close() {
        if (!open) {
            return;
        }
        synchronized (this) {
            if (!open) {
                return;
            }
            open = false;

            for (final EntityManager m : em) {
                if (m.isOpen()) {
                    m.close();
                }
            }
            em.clear();
            if (serverSession != null) {
                serverSession.close();
                this.serverSession = null;
            }
        }
    }

    public EntityManager createEntityManager() {
        return this.createEntityManager(Collections.<String, String>emptyMap());
    }

    public EntityManager createEntityManager(Map<String, String> map) {
        if (!open) {
            throw new IllegalStateException("The OWLEntityManager has been closed.");
        }

        final Map<String, String> newMap = new HashMap<>(map);

        newMap.putAll(configuration.getProperties());

        initServerSession();

        final AbstractEntityManager c = new EntityManagerImpl(this, new Configuration(newMap), this.serverSession);

        em.add(c);
        return c;
    }

    /**
     * Initializes the server session if necessary.
     */
    private synchronized void initServerSession() {
        if (serverSession == null) {
            this.serverSession = new ServerSession(storageProperties, configuration.getProperties(), metamodel);
        }
    }

    /**
     * The server session should by initialized by now, but to make sure, there is default initialization with an empty
     * properties map.
     *
     * @return The ServerSession for this factory.
     */
    public ServerSession getServerSession() {
        return serverSession;
    }

    public boolean isOpen() {
        return open;
    }

    public Map<String, String> getProperties() {
        return configuration.getProperties();
    }

    public Set<AbstractEntityManager> getEntityManagers() {
        return Collections.unmodifiableSet(em);
    }

    public Metamodel getMetamodel() {
        return metamodel;
    }

    public PersistenceUnitUtil getPersistenceUnitUtil() {
        return this;
    }

    public Object getIdentifier(Object entity) {
        try {
            return getMetamodel().entity(entity.getClass()).getIdentifier().getJavaField()
                                 .get(entity);
        } catch (IllegalArgumentException | IllegalAccessException e) {
            throw new OWLPersistenceException(e);
        }
    }

    public boolean isLoaded(Object entity, String attributeName) {
        for (final AbstractEntityManager emi : em) {
            if (emi.contains(entity)) {
                return attributeName == null || emi.isLoaded(entity, attributeName);

            }
        }

        return false;
    }

    public boolean isLoaded(Object entity) {
        // TODO
        return false;
        // return isLoaded(entity);
    }

    public Cache getCache() {
        if (!isOpen()) {
            throw new IllegalStateException("The entity manager factory is closed.");
        }
        initServerSession();
        return serverSession.getLiveObjectCache();
    }

}
