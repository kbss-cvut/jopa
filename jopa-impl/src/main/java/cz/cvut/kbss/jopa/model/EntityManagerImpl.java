/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.exceptions.TransactionRequiredException;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.query.criteria.CriteriaParameterFiller;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import cz.cvut.kbss.jopa.transactions.EntityTransactionWrapper;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.Wrapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class EntityManagerImpl implements AbstractEntityManager, Wrapper {

    private static final Logger LOG = LoggerFactory.getLogger(EntityManagerImpl.class);

    private static final Object MAP_VALUE = new Object();

    private final EntityManagerFactoryImpl emf;
    private final ServerSession serverSession;

    private boolean open;

    private final EntityTransactionWrapper transaction;
    private UnitOfWork persistenceContext;
    private final Configuration configuration;

    private Map<Object, Object> cascadingRegistry = new IdentityHashMap<>();

    EntityManagerImpl(EntityManagerFactoryImpl emf, Configuration configuration, ServerSession serverSession) {
        this.emf = emf;
        this.serverSession = serverSession;
        this.configuration = configuration;

        this.transaction = new EntityTransactionWrapper(this);

        this.open = true;
    }

    @Override
    public void persist(final Object entity) {
        final Descriptor d = new EntityDescriptor();
        persist(entity, d);
    }

    @Override
    public void persist(final Object entity, final Descriptor descriptor) {
        LOG.trace("Persisting instance of type {}.", entity.getClass());
        try {
            Objects.requireNonNull(entity);
            Objects.requireNonNull(descriptor);
            ensureOpen();
            checkClassIsValidEntity(entity.getClass());

            switch (getState(entity, descriptor)) {
                case NOT_MANAGED:
                    getCurrentPersistenceContext().registerNewObject(entity, descriptor);
                    // Intentional fall-through
                case MANAGED:
                    cascadePersist(entity, descriptor);
                    break;
                case REMOVED:
                    getCurrentPersistenceContext().restoreRemovedObject(entity);
                    break;
                default:
                    break;
            }
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
    }

    private void checkClassIsValidEntity(Class<?> cls) {
        getMetamodel().entity(cls);
    }

    private void registerProcessedInstance(Object instance) {
        cascadingRegistry.put(instance, MAP_VALUE);
    }

    private boolean isCascadingCycle(Object instance) {
        return cascadingRegistry.containsKey(instance);
    }

    private void resetCascadingRegistry() {
        cascadingRegistry.clear();
    }

    private void markTransactionForRollback() {
        if (getTransaction().isActive()) {
            getTransaction().setRollbackOnly();
        }
    }

    private void cascadePersist(final Object entity, final Descriptor descriptor) {
        new OneLevelCascadeExplorer() {
            @Override
            protected void exploreCascaded(Attribute<?, ?> at, Object o) {
                try {
                    Object ox = EntityPropertiesUtils.getAttributeValue(at, o);
                    LOG.trace("object={}, attribute={}, value={}", o, at.getName(), ox);
                    if (ox == null) {
                        return;
                    }
                    final Descriptor attDescriptor = descriptor.getAttributeDescriptor(at);
                    if (at.isCollection()) {
                        for (final Object ox2 : (Collection<?>) ox) {
                            persist(ox2, attDescriptor);
                        }
                    } else {
                        persist(ox, attDescriptor);
                    }
                } catch (Exception e) {
                    markTransactionForRollback();
                    throw new OWLPersistenceException(
                            "A problem occurred when persisting attribute " + at.getName()
                                    + " of with value " + o + " of object " + entity, e);
                }
            }
        }.start(this, entity, CascadeType.PERSIST);
    }

    @Override
    public <T> T merge(final T entity) {
        final Descriptor d = new EntityDescriptor();
        return merge(entity, d);
    }

    @Override
    public <T> T merge(final T entity, final Descriptor descriptor) {
        try {
            Objects.requireNonNull(entity);
            Objects.requireNonNull(descriptor);
            ensureOpen();
            checkClassIsValidEntity(entity.getClass());

            return mergeInternal(entity, descriptor);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        } finally {
            resetCascadingRegistry();
        }
    }

    /**
     * Merges state of the specified entity into the current persistence context. </p>
     *
     * @param entity     Entity instance
     * @param descriptor Contains information about contexts into which the entity and its field should be merged
     * @return Managed instance of the merged entity
     */
    private <T> T mergeInternal(final T entity, final Descriptor descriptor) {
        assert entity != null;
        assert descriptor != null;
        LOG.trace("Merging instance of type {}.", entity.getClass());
        if (isCascadingCycle(entity)) {
            LOG.warn("Merge cascading cycle detected in entity {}.", entity);
            return (T) getCurrentPersistenceContext().getCloneForOriginal(entity);
        }

        switch (getState(entity, descriptor)) {
            case MANAGED_NEW:
            case MANAGED:
                registerProcessedInstance(entity);
                cascadeMerge(entity, entity, descriptor);
                return entity;
            case NOT_MANAGED:
                final T merged = getCurrentPersistenceContext().mergeDetached(entity, descriptor);
                registerProcessedInstance(entity);
                cascadeMerge(merged, entity, descriptor);
                return merged;
            case REMOVED:
            default:
                throw new IllegalArgumentException("Cannot merge instance which is not an entity or is removed.");
        }
    }

    private <T> void cascadeMerge(T merged, T toMerge, Descriptor descriptor) {
        new OneLevelMergeCascadeExplorer() {
            @Override
            protected void exploreCascaded(Attribute<?, ?> at, Object merged, Object toMerge) {
                final Descriptor attDescriptor = descriptor.getAttributeDescriptor(at);
                mergeX(at, merged, toMerge, attDescriptor);
            }
        }.start(this, merged, toMerge);
    }

    private void mergeX(Attribute<?, ?> at, Object merged, Object toMerge, Descriptor descriptor) {
        Object attVal = EntityPropertiesUtils.getAttributeValue(at, toMerge);
        if (attVal == null) {
            return;
        }
        if (at.isCollection()) {
            Collection c = (Collection) attVal;
            Collection result = CollectionFactory.createInstance(c);
            for (final Object ox2 : c) {
                result.add(mergeInternal(ox2, descriptor));
            }
            attVal = getCurrentPersistenceContext().createIndirectCollection(result, merged, at.getJavaField());
        } else {
            attVal = mergeInternal(attVal, descriptor);
        }
        EntityPropertiesUtils.setFieldValue(at.getJavaField(), merged, attVal);
    }

    @Override
    public void remove(Object object) {
        try {
            ensureOpen();
            Objects.requireNonNull(object);
            checkClassIsValidEntity(object.getClass());
            if (isCascadingCycle(object)) {
                LOG.warn("Remove cascading cycle detected in instance {}.", object);
                return;
            }

            switch (getState(object)) {
                case MANAGED_NEW:
                case MANAGED:
                    getCurrentPersistenceContext().removeObject(object);
                    registerProcessedInstance(object);
                    // Intentional fall-through
                case REMOVED:
                    new SimpleOneLevelCascadeExplorer(this::remove).start(this, object, CascadeType.REMOVE);
                    break;
                default:
                    throw new IllegalArgumentException("Entity " + object + " is not managed and cannot be removed.");
            }
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        } finally {
            resetCascadingRegistry();
        }
    }

    @Override
    public <T> T find(Class<T> cls, Object identifier) {
        final EntityDescriptor d = new EntityDescriptor();
        return find(cls, identifier, d);
    }

    @Override
    public <T> T find(Class<T> cls, Object identifier, Descriptor descriptor) {
        try {
            Objects.requireNonNull(cls);
            Objects.requireNonNull(identifier);
            Objects.requireNonNull(descriptor);
            ensureOpen();
            checkClassIsValidEntity(cls);

            LOG.trace("Finding instance of {} with identifier {} in context {}.", cls, identifier, descriptor);
            final URI uri = (identifier instanceof URI) ? (URI) identifier : URI.create(identifier.toString());

            return getCurrentPersistenceContext().readObject(cls, uri, descriptor);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
    }

    @Override
    public <T> T getReference(Class<T> entityClass, Object identifier) {
        try {
            Objects.requireNonNull(entityClass);
            Objects.requireNonNull(identifier);

            return getReference(entityClass, identifier, new EntityDescriptor());
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
    }

    @Override
    public <T> T getReference(Class<T> entityClass, Object identifier, Descriptor descriptor) {
        try {
            Objects.requireNonNull(entityClass);
            Objects.requireNonNull(identifier);
            Objects.requireNonNull(descriptor);
            ensureOpen();
            checkClassIsValidEntity(entityClass);

            LOG.trace("Getting reference of type {} with identifier {} in context {}.", entityClass, identifier,
                    descriptor);
            return getCurrentPersistenceContext().getReference(entityClass, identifier, descriptor);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
    }

    @Override
    public void flush() {
        try {
            ensureOpen();
            LOG.trace("Flushing changes...");
            if (!getTransaction().isActive()) {
                throw new TransactionRequiredException("Cannot flush entity manager outside of a transaction.");
            }
            this.getCurrentPersistenceContext().writeUncommittedChanges();
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
    }

    @Override
    public void refresh(Object entity) {
        try {
            ensureOpen();
            Objects.requireNonNull(entity);
            checkClassIsValidEntity(entity.getClass());

            this.getCurrentPersistenceContext().refreshObject(entity);
            new SimpleOneLevelCascadeExplorer(this::refresh).start(this, entity, CascadeType.REFRESH);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
    }

    @Override
    public void clear() {
        try {
            ensureOpen();
            getCurrentPersistenceContext().clear();
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
    }

    @Override
    public void detach(Object entity) {
        try {
            ensureOpen();

            switch (getState(entity)) {
                case MANAGED_NEW:
                case MANAGED:
                    getCurrentPersistenceContext().unregisterObject(entity);
                    new SimpleOneLevelCascadeExplorer(this::detach).start(this, entity, CascadeType.DETACH);
                    break;
                default:
                    break;
            }
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
    }

    @Override
    public boolean contains(Object entity) {
        try {
            ensureOpen();
            Objects.requireNonNull(entity);
            checkClassIsValidEntity(entity.getClass());
            return getCurrentPersistenceContext().contains(entity);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
    }

    @Override
    public void close() {
        ensureOpen();
        removeCurrentPersistenceContext();
        this.cascadingRegistry = null;
        emf.entityManagerClosed(this);
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public EntityTransaction getTransaction() {
        return transaction.getTransaction();
    }

    @Override
    public EntityManagerFactoryImpl getEntityManagerFactory() {
        return emf;
    }

    @Override
    public Metamodel getMetamodel() {
        return emf.getMetamodel();
    }

    @Override
    public boolean isLoaded(final Object object, final String attributeName) {
        Objects.requireNonNull(object);
        Objects.requireNonNull(attributeName);
        return getCurrentPersistenceContext().isLoaded(object, attributeName) == LoadState.LOADED;
    }

    @Override
    public boolean isLoaded(Object object) {
        return getCurrentPersistenceContext().isLoaded(object) == LoadState.LOADED;
    }

    @Override
    public QueryImpl createQuery(String qlString) {
        ensureOpen();
        final QueryImpl q = getCurrentPersistenceContext().sparqlQueryFactory().createQuery(qlString);
        q.setRollbackOnlyMarker(this::markTransactionForRollback);
        q.setEnsureOpenProcedure(this::ensureOpen);
        return q;
    }

    @Override
    public <T> TypedQuery<T> createQuery(CriteriaQuery<T> criteriaQuery) {
        ensureOpen();
        CriteriaQueryImpl<T> query = (CriteriaQueryImpl<T>) criteriaQuery;
        CriteriaParameterFiller parameterFiller = new CriteriaParameterFiller();
        String soqlQuery = query.translateQuery(parameterFiller);
        LOG.debug("CriteriaQuery translate to SOQL query: " + soqlQuery);
        final TypedQueryImpl<T> q = getCurrentPersistenceContext().sparqlQueryFactory().createQuery(soqlQuery, query.getResultType());
        q.setRollbackOnlyMarker(this::markTransactionForRollback);
        q.setEnsureOpenProcedure(this::ensureOpen);
        parameterFiller.setValuesToRegisteredParameters(q);

        return q;
    }

    @Override
    public <T> TypedQueryImpl<T> createQuery(String query, Class<T> resultClass) {
        ensureOpen();
        final TypedQueryImpl<T> q = getCurrentPersistenceContext().sparqlQueryFactory().createQuery(query, resultClass);
        q.setRollbackOnlyMarker(this::markTransactionForRollback);
        q.setEnsureOpenProcedure(this::ensureOpen);
        return q;
    }

    @Override
    public QueryImpl createNativeQuery(String sparqlString) {
        ensureOpen();
        final QueryImpl q = getCurrentPersistenceContext().sparqlQueryFactory().createNativeQuery(sparqlString);
        q.setRollbackOnlyMarker(this::markTransactionForRollback);
        q.setEnsureOpenProcedure(this::ensureOpen);
        return q;
    }

    @Override
    public <T> TypedQueryImpl<T> createNativeQuery(String sparqlString, Class<T> resultClass) {
        ensureOpen();
        final TypedQueryImpl<T> q = getCurrentPersistenceContext().sparqlQueryFactory()
                                                                  .createNativeQuery(sparqlString, resultClass);
        q.setRollbackOnlyMarker(this::markTransactionForRollback);
        q.setEnsureOpenProcedure(this::ensureOpen);
        return q;
    }

    @Override
    public QueryImpl createNativeQuery(String sparqlString, String resultSetMapping) {
        ensureOpen();
        final QueryImpl q = getCurrentPersistenceContext().sparqlQueryFactory()
                                                          .createNativeQuery(sparqlString, resultSetMapping);
        q.setRollbackOnlyMarker(this::markTransactionForRollback);
        q.setEnsureOpenProcedure(this::ensureOpen);
        return q;
    }

    @Override
    public QueryImpl createNamedQuery(String name) {
        ensureOpen();
        final QueryImpl q = getCurrentPersistenceContext().sparqlQueryFactory().createNamedQuery(name);
        q.setRollbackOnlyMarker(this::markTransactionForRollback);
        q.setEnsureOpenProcedure(this::ensureOpen);
        return q;
    }

    @Override
    public <T> TypedQueryImpl<T> createNamedQuery(String name, Class<T> resultClass) {
        ensureOpen();
        final TypedQueryImpl<T> q = getCurrentPersistenceContext().sparqlQueryFactory()
                                                                  .createNamedQuery(name, resultClass);
        q.setRollbackOnlyMarker(this::markTransactionForRollback);
        q.setEnsureOpenProcedure(this::ensureOpen);
        return q;
    }

    @Override
    public boolean isConsistent(URI context) {
        ensureOpen();
        return getCurrentPersistenceContext().isConsistent(context);
    }

    @Override
    public List<URI> getContexts() {
        ensureOpen();
        return getCurrentPersistenceContext().getContexts();
    }

    @Override
    public <T> boolean isInferred(T entity, FieldSpecification<? super T, ?> attribute, Object value) {
        ensureOpen();
        return getCurrentPersistenceContext().isInferred(entity, attribute, value);
    }

    @Override
    public CriteriaBuilder getCriteriaBuilder() {
        return getCurrentPersistenceContext().getCriteriaBuilder();
    }

    @Override
    public <T> T unwrap(Class<T> cls) {
        ensureOpen();
        if (cls.isAssignableFrom(this.getClass())) {
            return cls.cast(this);
        }
        return getCurrentPersistenceContext().unwrap(cls);
    }

    @Override
    public EntityManagerImpl getDelegate() {
        return unwrap(EntityManagerImpl.class);
    }

    private void ensureOpen() {
        if (!isOpen()) {
            throw new IllegalStateException("The entity manager is closed!");
        }
    }

    private EntityState getState(Object entity) {
        return getCurrentPersistenceContext().getState(entity);
    }

    private EntityState getState(Object entity, Descriptor descriptor) {
        return getCurrentPersistenceContext().getState(entity, descriptor);
    }

    @Override
    protected void finalize() throws Throwable {
        if (open) {
            close();
        }
        super.finalize();
    }

    @Override
    public UnitOfWork getCurrentPersistenceContext() {
        if (persistenceContext == null) {
            this.persistenceContext = serverSession.acquireUnitOfWork(configuration);
        }
        return persistenceContext;
    }

    /**
     * Called from EntityTransaction in case of a rollback. Releasing the UoW is up to the EntityTransaction.
     */
    @Override
    public void removeCurrentPersistenceContext() {
        if (persistenceContext != null && persistenceContext.isActive()) {
            persistenceContext.release();
        }
        this.persistenceContext = null;
    }

    @Override
    public void transactionStarted(EntityTransaction t) {
        serverSession.transactionStarted(t, this);
    }

    @Override
    public void transactionFinished(EntityTransaction t) {
        this.serverSession.transactionFinished(t);
    }

    @Override
    public Configuration getConfiguration() {
        return configuration;
    }

    @Override
    public Map<String, Object> getProperties() {
        return new HashMap<>(configuration.getProperties());
    }

    @Override
    public void setProperty(String propertyName, Object value) {
        Objects.requireNonNull(propertyName);
        Objects.requireNonNull(value);
        configuration.set(propertyName, value.toString());
    }
}
