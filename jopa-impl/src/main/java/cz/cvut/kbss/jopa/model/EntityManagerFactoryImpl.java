/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.loaders.PersistenceUnitClassFinder;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

public class EntityManagerFactoryImpl implements EntityManagerFactory, PersistenceUnitUtil {

    private volatile boolean open = true;

    private final Set<AbstractEntityManager> em;
    private final Configuration configuration;
    private final OntologyStorageProperties storageProperties;

    private ServerSession serverSession;

    private final MetamodelImpl metamodel;

    private final EntityDescriptorFactory descriptorFactory;

    private final Consumer<EntityManagerFactoryImpl> closeListener;

    public EntityManagerFactoryImpl(final Map<String, String> properties,
                                    Consumer<EntityManagerFactoryImpl> closeListener) {
        this.em = Collections.newSetFromMap(new ConcurrentHashMap<>());
        this.configuration = new Configuration(properties != null ? properties : Collections.emptyMap());
        this.closeListener = closeListener;
        this.storageProperties = initStorageProperties();
        this.metamodel = initMetamodel();
        this.descriptorFactory = initDescriptorFactory();
    }

    private OntologyStorageProperties initStorageProperties() {
        return OntologyStorageProperties.driver(configuration.get(JOPAPersistenceProperties.DATA_SOURCE_CLASS))
                                        .ontologyUri(configuration.get(JOPAPersistenceProperties.ONTOLOGY_URI_KEY))
                                        .physicalUri(
                                                configuration.get(JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY))
                                        .username(configuration.get(OntoDriverProperties.DATA_SOURCE_USERNAME))
                                        .password(configuration.get(OntoDriverProperties.DATA_SOURCE_PASSWORD))
                                        .build();
    }

    private MetamodelImpl initMetamodel() {
        final MetamodelImpl metamodel = new MetamodelImpl(configuration);
        metamodel.build(new PersistenceUnitClassFinder());
        return metamodel;
    }

    private EntityDescriptorFactory initDescriptorFactory() {
        return new CachingEntityDescriptorFactory(new DefaultEntityDescriptorFactory(metamodel, metamodel.getNamespaceResolver()));
    }

    @Override
    public void close() {
        ensureOpen();
        synchronized (this) {
            if (!open) {
                return;
            }

            em.stream().filter(EntityManager::isOpen).forEach(EntityManager::close);
            em.clear();
            if (serverSession != null) {
                serverSession.close();
                this.serverSession = null;
            }
            this.open = false;
        }
        closeListener.accept(this);
    }

    @Override
    public EntityManager createEntityManager() {
        return this.createEntityManager(Collections.emptyMap());
    }

    @Override
    public EntityManager createEntityManager(Map<String, String> map) {
        ensureOpen();
        initServerSession();

        final Map<String, String> newMap = new HashMap<>(map);
        newMap.putAll(configuration.getProperties());
        final AbstractEntityManager c = new EntityManagerImpl(this, new Configuration(newMap), descriptorFactory);
        em.add(c);
        return c;
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The entity manager factory is closed.");
        }
    }

    /**
     * Initializes the server session if necessary.
     */
    private synchronized void initServerSession() {
        if (serverSession == null) {
            this.serverSession = new ServerSession(storageProperties, configuration, metamodel);
        }
    }

    @Override
    public CriteriaBuilder getCriteriaBuilder() {
        ensureOpen();
        initServerSession();
        return serverSession.getCriteriaBuilder();
    }

    /**
     * Gets the {@link ServerSession} that provides storage access and second level cache in this persistence unit.
     *
     * @return The ServerSession for this factory.
     * @throws IllegalStateException If the factory is closed
     */
    public ServerSession getServerSession() {
        ensureOpen();
        initServerSession();
        return serverSession;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public Map<String, String> getProperties() {
        ensureOpen();
        return configuration.getProperties();
    }

    @Override
    public MetamodelImpl getMetamodel() {
        ensureOpen();
        return metamodel;
    }

    @Override
    public PersistenceUnitUtil getPersistenceUnitUtil() {
        ensureOpen();
        return this;
    }

    @Override
    public void addNamedQuery(String name, Query query) {
        ensureOpen();
        throw new UnsupportedOperationException("Not supported, yet.");
    }

    @Override
    public <T> T unwrap(Class<T> cls) {
        ensureOpen();
        if (cls.isAssignableFrom(this.getClass())) {
            return cls.cast(this);
        }
        return serverSession.unwrap(cls);
    }

    @Override
    public Object getIdentifier(Object entity) {
        Objects.requireNonNull(entity);
        final EntityType<?> et = getMetamodel().entity(entity.getClass());
        return EntityPropertiesUtils.getFieldValue(et.getIdentifier().getJavaField(), entity);
    }

    @Override
    public boolean isLoaded(Object entity, String attributeName) {
        Objects.requireNonNull(entity);
        Objects.requireNonNull(attributeName);
        for (final AbstractEntityManager emi : em) {
            if (emi.isLoaded(entity, attributeName)) {
                return true;
            }
        }
        return false;
    }


    @Override
    public boolean isLoaded(Object entity) {
        Objects.requireNonNull(entity);
        return em.stream().anyMatch(emi -> emi.isLoaded(entity));
    }

    @Override
    public Cache getCache() {
        ensureOpen();
        initServerSession();
        return serverSession.getLiveObjectCache();
    }

    void entityManagerClosed(AbstractEntityManager manager) {
        assert manager != null;
        em.remove(manager);
    }
}
