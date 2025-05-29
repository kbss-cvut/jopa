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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.accessors.DefaultStorageAccessor;
import cz.cvut.kbss.jopa.accessors.StorageAccessor;
import cz.cvut.kbss.jopa.model.AbstractEntityManager;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.cache.CacheManager;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.query.criteria.CriteriaBuilderImpl;
import cz.cvut.kbss.jopa.sessions.cache.CacheFactory;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import cz.cvut.kbss.jopa.utils.ChangeTrackingMode;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.Constants;
import cz.cvut.kbss.jopa.utils.Wrapper;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The ServerSession is the primary interface for accessing the ontology.
 * <p>
 * It manages an accessor object, which performs the queries.
 */
public class ServerSession extends AbstractSession implements Wrapper {

    private static final Logger LOG = LoggerFactory.getLogger(ServerSession.class);

    private final MetamodelImpl metamodel;

    private CacheManager liveObjectCache;
    private StorageAccessor storageAccessor;
    private final CriteriaBuilder criteriaBuilder;

    private Map<EntityTransaction, AbstractEntityManager> runningTransactions;

    ServerSession(MetamodelImpl metamodel) {
        super(new Configuration());
        this.metamodel = metamodel;
        this.criteriaBuilder = new CriteriaBuilderImpl(metamodel);
    }

    public ServerSession(OntologyStorageProperties storageProperties, Configuration configuration,
                         MetamodelImpl metamodel) {
        super(configuration);
        this.metamodel = metamodel;
        this.criteriaBuilder = new CriteriaBuilderImpl(metamodel);
        initialize(storageProperties, configuration, metamodel);
    }

    /**
     * Initializes this ServerSession. This in particular means initialization of the ontology accessor and live object
     * cache.
     *
     * @param storageProperties Storage properties
     * @param configuration     Session configuration
     * @param metamodel         Metamodel of the managed classes and their attributes.
     */
    private void initialize(OntologyStorageProperties storageProperties, Configuration configuration,
                            Metamodel metamodel) {
        assert configuration != null;
        assert metamodel != null;
        this.runningTransactions = new ConcurrentHashMap<>();
        this.liveObjectCache = CacheFactory.createCache(configuration.getProperties());
        liveObjectCache.setInferredClasses(metamodel.getInferredClasses());
        this.storageAccessor = new DefaultStorageAccessor(storageProperties, configuration.getProperties());
    }

    /**
     * Acquires a {@link UnitOfWork} instance to perform transactional operations.
     *
     * @return UnitOfWork instance
     */
    public UnitOfWork acquireUnitOfWork(Configuration configuration) {
        final String enabledStr = configuration.get(JOPAPersistenceProperties.TRANSACTION_MODE);
        if (Constants.READ_ONLY_TRANSACTION_MODE.equals(enabledStr)) {
            LOG.trace("Acquiring read-only UnitOfWork.");
            return new ReadOnlyUnitOfWork(this, configuration);
        }

        final ChangeTrackingMode mode = ChangeTrackingMode.resolve(configuration);
        return switch (mode) {
            case IMMEDIATE -> {
                LOG.trace("Acquiring change tracking UnitOfWork.");
                yield new ChangeTrackingUnitOfWork(this, configuration);
            }
            case ON_COMMIT -> {
                LOG.trace("Acquiring on commit change calculating UnitOfWork.");
                yield new OnCommitChangePropagatingUnitOfWork(this, configuration);
            }
        };
    }

    @Override
    protected ConnectionWrapper acquireConnection() {
        return new ConnectionWrapper(storageAccessor.acquireConnection());
    }

    @Override
    public CacheManager getLiveObjectCache() {
        return liveObjectCache;
    }

    public void transactionStarted(EntityTransaction t, AbstractEntityManager em) {
        assert t.isActive();
        runningTransactions.put(t, em);
    }

    public void transactionFinished(EntityTransaction t) {
        if (t == null) {
            return;
        }
        runningTransactions.remove(t);
    }

    /**
     * Close the server session and all connections to the underlying data source.
     */
    public void close() {
        if (!runningTransactions.isEmpty()) {
            LOG.warn("There are still transactions running. Marking them for rollback.");
            runningTransactions.keySet().stream().filter(EntityTransaction::isActive)
                               .forEach(EntityTransaction::setRollbackOnly);
        }
        if (storageAccessor != null && storageAccessor.isOpen()) {
            try {
                storageAccessor.close();
            } catch (OntoDriverException e) {
                LOG.error("Exception caught when closing the storage accessor.", e);
            }
        }
        liveObjectCache.close();
    }

    @Override
    public MetamodelImpl getMetamodel() {
        return metamodel;
    }

    @Override
    public boolean isEntityType(Class<?> cls) {
        return metamodel.isEntityType(cls);
    }

    @Override
    public CriteriaBuilder getCriteriaBuilder() {
        return criteriaBuilder;
    }

    @Override
    public <T> T unwrap(Class<T> cls) {
        Objects.requireNonNull(cls);
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        } else if (cls.isAssignableFrom(liveObjectCache.getClass())) {
            return cls.cast(liveObjectCache);
        }
        return storageAccessor.unwrap(cls);
    }
}
