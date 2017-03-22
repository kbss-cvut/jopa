/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.exceptions.TransactionRequiredException;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import cz.cvut.kbss.jopa.transactions.EntityTransactionWrapper;
import cz.cvut.kbss.jopa.transactions.TransactionWrapper;
import cz.cvut.kbss.jopa.utils.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

public class EntityManagerImpl extends AbstractEntityManager implements Wrapper {

    private static final Logger LOG = LoggerFactory.getLogger(EntityManagerImpl.class);

    private EntityManagerFactoryImpl emf;

    private boolean open;

    private TransactionWrapper transaction;
    private UnitOfWorkImpl persistenceContext;
    private ServerSession serverSession;
    private final Configuration configuration;

    public EntityManagerImpl(EntityManagerFactoryImpl emf, Configuration configuration, ServerSession serverSession) {
        this.emf = emf;
        this.serverSession = serverSession;
        this.configuration = configuration;

        setTransactionWrapper();

        this.open = true;
    }

    public enum State {
        MANAGED, MANAGED_NEW, NOT_MANAGED, REMOVED
    }

    @Override
    public void persist(final Object entity) {
        final Descriptor d = new EntityDescriptor();
        persist(entity, d);
    }

    @Override
    public void persist(final Object entity, final Descriptor descriptor) {
        LOG.trace("Persisting {}", entity);
        try {
            Objects.requireNonNull(entity, ErrorUtils.getNPXMessageSupplier("entity"));
            Objects.requireNonNull(descriptor, ErrorUtils.getNPXMessageSupplier("descriptor"));
            ensureOpen();
            checkClassIsValidEntity(entity.getClass());

            switch (getState(entity, descriptor)) {
                case NOT_MANAGED:
                    try {
                        getCurrentPersistenceContext().registerNewObject(entity, descriptor);
                    } catch (RuntimeException e) {
                        markTransactionForRollback();
                        throw e;
                    }
                case MANAGED:
                    cascadePersist(entity, descriptor);
                    break;
                case REMOVED:
                    getCurrentPersistenceContext().revertObject(entity);
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
            Objects.requireNonNull(entity, ErrorUtils.getNPXMessageSupplier("entity"));
            Objects.requireNonNull(descriptor, ErrorUtils.getNPXMessageSupplier("descriptor"));
            ensureOpen();
            checkClassIsValidEntity(entity.getClass());

            return mergeInternal(entity, descriptor);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
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
        LOG.trace("Merging {}.", entity);

        switch (getState(entity, descriptor)) {
            case MANAGED_NEW:
            case MANAGED:
                new OneLevelMergeCascadeExplorer() {
                    @Override
                    protected void exploreCascaded(Attribute<?, ?> at, Object merged, Object toMerge) {
                        final Descriptor attDescriptor = descriptor.getAttributeDescriptor(at);
                        mergeX(at, merged, toMerge, attDescriptor);
                    }
                }.start(this, entity, entity);
                return entity;
            case NOT_MANAGED:
                final T merged;
                merged = getCurrentPersistenceContext().mergeDetached(entity, descriptor);

                new OneLevelMergeCascadeExplorer() {
                    @Override
                    protected void exploreCascaded(Attribute<?, ?> at, Object merged, Object toMerge) {
                        final Descriptor attDescriptor = descriptor.getAttributeDescriptor(at);
                        mergeX(at, merged, toMerge, attDescriptor);
                    }
                }.start(this, merged, entity);
                return merged;
            case REMOVED:
            default:
                throw new IllegalArgumentException();
        }
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

            switch (getState(object)) {
                case MANAGED_NEW:
                case MANAGED:
                    getCurrentPersistenceContext().removeObject(object);
                    // Intentional fall-through
                case REMOVED:
                    new SimpleOneLevelCascadeExplorer() {
                        @Override
                        protected void runCascadedForEach(Object ox2) {
                            remove(ox2);
                        }
                    }.start(this, object, CascadeType.REMOVE);
                    break;
                default:
                    throw new IllegalArgumentException("Entity " + object + " is not managed and cannot be removed.");
            }
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        }
    }

    @Override
    public <T> T find(Class<T> cls, Object primaryKey) {
        final EntityDescriptor d = new EntityDescriptor();
        return find(cls, primaryKey, d);
    }

    @Override
    public <T> T find(Class<T> cls, Object primaryKey, Descriptor descriptor) {
        try {
            Objects.requireNonNull(cls, ErrorUtils.getNPXMessageSupplier("cls"));
            Objects.requireNonNull(primaryKey, ErrorUtils.getNPXMessageSupplier("primaryKey"));
            Objects.requireNonNull(descriptor, ErrorUtils.getNPXMessageSupplier("descriptor"));
            ensureOpen();
            checkClassIsValidEntity(cls);

            LOG.trace("Finding instance of {} with identifier {} in context ", cls, primaryKey, descriptor);
            final URI uri = (primaryKey instanceof URI) ? (URI) primaryKey : URI.create(primaryKey.toString());

            return getCurrentPersistenceContext().readObject(cls, uri, descriptor);
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
                throw new TransactionRequiredException();
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

            this.getCurrentPersistenceContext().revertObject(entity);
            new SimpleOneLevelCascadeExplorer() {
                @Override
                protected void runCascadedForEach(Object ox2) {
                    refresh(ox2);
                }
            }.start(this, entity, CascadeType.REFRESH);
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
                    new SimpleOneLevelCascadeExplorer() {
                        @Override
                        protected void runCascadedForEach(Object ox2) {
                            detach(ox2);
                        }
                    }.start(this, entity, CascadeType.DETACH);
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
        emf.entityManagerClosed(this);
        open = false;
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
        // TODO
        return false;
    }

    @Override
    public Query createQuery(String qlString) {
        final QueryImpl q = getCurrentPersistenceContext().createQuery(qlString);
        q.setRollbackOnlyMarker(this::markTransactionForRollback);
        return q;
    }

    @Override
    public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass) {
        final TypedQueryImpl<T> q = getCurrentPersistenceContext().createQuery(query, resultClass);
        q.setRollbackOnlyMarker(this::markTransactionForRollback);
        return q;
    }

    @Override
    public Query createNativeQuery(String sparql) {
        final QueryImpl q = getCurrentPersistenceContext().createNativeQuery(sparql);
        q.setRollbackOnlyMarker(this::markTransactionForRollback);
        return q;
    }

    @Override
    public <T> TypedQuery<T> createNativeQuery(String sparql, Class<T> resultClass) {
        final TypedQueryImpl<T> q = getCurrentPersistenceContext().createNativeQuery(sparql, resultClass);
        q.setRollbackOnlyMarker(this::markTransactionForRollback);
        return q;
    }

    @Override
    public Query createNamedQuery(String name) {
        final QueryImpl q = getCurrentPersistenceContext().createNamedQuery(name);
        q.setRollbackOnlyMarker(this::markTransactionForRollback);
        return q;
    }

    @Override
    public <T> TypedQuery<T> createNamedQuery(String name, Class<T> resultClass) {
        final TypedQueryImpl<T> q = getCurrentPersistenceContext().createNamedQuery(name, resultClass);
        q.setRollbackOnlyMarker(this::markTransactionForRollback);
        return q;
    }

    @Override
    public boolean isConsistent(URI context) {
        return getCurrentPersistenceContext().isConsistent(context);
    }

    @Override
    public List<URI> getContexts() {
        return getCurrentPersistenceContext().getContexts();
    }

    @Override
    public void setUseTransactionalOntologyForQueryProcessing() {
        getCurrentPersistenceContext().setUseTransactionalOntologyForQueryProcessing();
    }

    @Override
    public boolean useTransactionalOntologyForQueryProcessing() {
        return getCurrentPersistenceContext().useTransactionalOntologyForQueryProcessing();
    }

    @Override
    public void setUseBackupOntologyForQueryProcessing() {
        getCurrentPersistenceContext().setUseBackupOntologyForQueryProcessing();
    }

    @Override
    public boolean useBackupOntologyForQueryProcessing() {
        return getCurrentPersistenceContext().useBackupOntologyForQueryProcessing();
    }

    @Override
    public <T> T unwrap(Class<T> cls) {
        if (cls.isAssignableFrom(this.getClass())) {
            return cls.cast(this);
        }
        return getCurrentPersistenceContext().unwrap(cls);
    }

    @Override
    public Object getDelegate() {
        return unwrap(EntityManagerImpl.class);
    }

    private void ensureOpen() {
        if (!isOpen()) {
            throw new OWLPersistenceException("The entity manager is closed !");
        }
    }

    private State getState(Object entity) {
        return getCurrentPersistenceContext().getState(entity);
    }

    private State getState(Object entity, Descriptor descriptor) {
        return getCurrentPersistenceContext().getState(entity, descriptor);
    }

    @Override
    protected void finalize() throws Throwable {
        if (isOpen()) {
            close();
        }
        super.finalize();
    }

    @Override
    public UnitOfWorkImpl getCurrentPersistenceContext() {
        if (this.persistenceContext == null) {
            this.persistenceContext = (UnitOfWorkImpl) this.serverSession.acquireUnitOfWork();
            persistenceContext.setEntityManager(this);
        }
        return this.persistenceContext;
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
        this.serverSession.transactionStarted(t, this);
    }

    @Override
    public void transactionFinished(EntityTransaction t) {
        this.serverSession.transactionFinished(t);
    }

    /**
     * Since we support only EntityTransactions, we set the TransactionWrapper to EntityTransactionWrapper.
     * <p>
     * In the future, if JTA transactions are supported, JTATransactionWrapper should be set instead of the
     * EntityTransactionWrapper.
     */
    private void setTransactionWrapper() {
        this.transaction = new EntityTransactionWrapper(this);
    }

    @Override
    public Configuration getConfiguration() {
        return configuration;
    }
}
