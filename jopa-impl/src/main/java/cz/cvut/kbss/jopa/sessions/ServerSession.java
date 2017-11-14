/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.accessors.DefaultStorageAccessor;
import cz.cvut.kbss.jopa.accessors.StorageAccessor;
import cz.cvut.kbss.jopa.model.AbstractEntityManager;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.Type;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;
import cz.cvut.kbss.jopa.sessions.cache.CacheFactory;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.Wrapper;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

/**
 * The ServerSession is the primary interface for accessing the ontology.
 * <p>
 * It manages an accessor object, which performs the queries.
 */
public class ServerSession extends AbstractSession implements Wrapper {

    private final MetamodelImpl metamodel;
    private final Set<Class<?>> managedClasses;

    private CacheManager liveObjectCache;
    private StorageAccessor storageAccessor;

    private Map<EntityTransaction, AbstractEntityManager> runningTransactions;

    ServerSession() {
        super(new Configuration(Collections.emptyMap()));
        this.metamodel = null;
        this.managedClasses = null;
    }

    public ServerSession(OntologyStorageProperties storageProperties, Configuration configuration,
                         MetamodelImpl metamodel) {
        super(configuration);
        this.metamodel = metamodel;
        this.managedClasses = processTypes(metamodel.getEntities());
        initialize(storageProperties, configuration, metamodel);
    }

    /**
     * Process the entity types and extract simple Java classes from them.
     *
     * @param entities Set of managed entity types.
     * @return Set of managed entity classes.
     */
    private Set<Class<?>> processTypes(Set<EntityType<?>> entities) {
        Set<Class<?>> types = new HashSet<>(entities.size());
        types.addAll(entities.stream().map(Type::getJavaType).collect(Collectors.toList()));
        return types;
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
        this.runningTransactions = new HashMap<>();
        this.liveObjectCache = CacheFactory.createCache(configuration.getProperties());
        liveObjectCache.setInferredClasses(metamodel.getInferredClasses());
        this.storageAccessor = new DefaultStorageAccessor(storageProperties, configuration.getProperties());
    }

    @Override
    protected ConnectionWrapper acquireConnection() {
        return new ConnectionWrapper(storageAccessor.acquireConnection());
    }

    @Override
    public UnitOfWork acquireUnitOfWork() {
        return new UnitOfWorkImpl(this);
    }

    @Override
    public CacheManager getLiveObjectCache() {
        return liveObjectCache;
    }

    public boolean transactionStarted(EntityTransaction t, AbstractEntityManager em) {
        if (!t.isActive() || t.isRollbackOnly()) {
            return false;
        }
        runningTransactions.put(t, em);
        return true;
    }

    public void transactionFinished(EntityTransaction t) {
        if (t == null) {
            return;
        }
        AbstractEntityManager em = runningTransactions.remove(t);
        if (em == null) {
            return;
        }
        UnitOfWorkImpl uow = (UnitOfWorkImpl) em.getCurrentPersistenceContext();
        if (uow != null && uow.hasChanges()) {
            getLiveObjectCache().clearInferredObjects();
        }
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
    public void removeObjectFromCache(Object object, URI context) {
        // do nothing
    }

    @Override
    public MetamodelImpl getMetamodel() {
        return metamodel;
    }

    @Override
    public boolean isTypeManaged(Class<?> cls) {
        return cls != null && managedClasses.contains(cls);
    }

    @Override
    public NamedQueryManager getNamedQueryManager() {
        return metamodel.getNamedQueryManager();
    }

    @Override
    public ResultSetMappingManager getResultSetMappingManager() {
        return metamodel.getResultSetMappingManager();
    }

    @Override
    public <T> T unwrap(Class<T> cls) {
        Objects.requireNonNull(cls);
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        }
        return storageAccessor.unwrap(cls);
    }
}
