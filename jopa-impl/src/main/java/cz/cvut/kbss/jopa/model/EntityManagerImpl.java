/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
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
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

public class EntityManagerImpl extends AbstractEntityManager {

    private static final Logger LOG = LoggerFactory.getLogger(EntityManagerImpl.class);

    private EntityManagerFactoryImpl emf;

    private boolean open;

    private TransactionWrapper transaction;
    private UnitOfWorkImpl persistenceContext;
    private ServerSession serverSession;
    private final Configuration configuration;

    public EntityManagerImpl(EntityManagerFactoryImpl emf, Configuration configuration,
                             ServerSession serverSession) {
        this.emf = emf;
        this.serverSession = serverSession;
        this.configuration = configuration;

        this.setTransactionWrapper();

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
        ensureOpen();
        Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
        Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

        switch (getState(entity, descriptor)) {
            case NOT_MANAGED:
                try {
                    getCurrentPersistenceContext().registerNewObject(entity, descriptor);
                } catch (RuntimeException e) {
                    if (getTransaction().isActive()) {
                        getTransaction().setRollbackOnly();
                    }
                    throw e;
                }
            case MANAGED:
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
                            if (getTransaction().isActive()) {
                                getTransaction().setRollbackOnly();
                            }
                            throw new OWLPersistenceException(
                                    "A problem occured when persisting attribute " + at.getName()
                                            + " of with value " + o + " of object " + entity, e);
                        }
                    }
                }.start(this, entity, CascadeType.PERSIST);
                break;
            case MANAGED_NEW:
                throw new OWLEntityExistsException("Entity " + entity
                        + " is already managed in this persistence context.");
            case REMOVED:
                getCurrentPersistenceContext().revertObject(entity);
                break;
        }
    }

    @Override
    public <T> T merge(final T entity) {
        final Descriptor d = new EntityDescriptor();
        return merge(entity, d);
    }

    @Override
    public <T> T merge(final T entity, final Descriptor descriptor) {
        Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
        Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

        return mergeInternal(entity, descriptor);
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
        LOG.trace("Merging {}", entity);
        ensureOpen();

        Class<T> clz = (Class<T>) entity.getClass();

        switch (getState(entity, descriptor)) {
            case MANAGED_NEW:
            case MANAGED:
                new OneLevelCascadeExplorer() {
                    @Override
                    protected void exploreCascaded(Attribute<?, ?> at, Object o) {
                        mergeX(at, o, descriptor);
                    }
                }.start(this, entity, CascadeType.MERGE);
                return entity;
            case NOT_MANAGED:
                final T merged;
                merged = getCurrentPersistenceContext().mergeDetached(entity, descriptor);

                new OneLevelCascadeExplorer() {
                    @Override
                    protected void exploreCascaded(Attribute<?, ?> at, Object o) {
                        final Descriptor attDescriptor = descriptor.getAttributeDescriptor(at);
                        mergeX(at, o, attDescriptor);
                    }

                    @Override
                    protected void exploreNonCascaded(Attribute<?, ?> at, Object o) {
                        final Object attVal = EntityPropertiesUtils.getAttributeValue(at, o);
                        EntityPropertiesUtils.setFieldValue(at.getJavaField(), o, attVal);
                    }
                }.start(this, merged, CascadeType.MERGE);
                return merged;
            case REMOVED:
            default:
                throw new IllegalArgumentException();
        }
    }

    private void mergeX(Attribute<?, ?> at, Object o, Descriptor descriptor) {
        Object attVal = EntityPropertiesUtils.getAttributeValue(at, o);
        if (attVal == null) {
            return;
        }
        if (at.isCollection()) {
            Collection c = (Collection) attVal;
            Collection merged = CollectionFactory.createInstance(c);
            for (final Object ox2 : c) {
                merged.add(mergeInternal(ox2, descriptor));
            }
            attVal = getCurrentPersistenceContext().createIndirectCollection(merged, o, at.getJavaField());
        } else {
            attVal = mergeInternal(attVal, descriptor);
        }
        EntityPropertiesUtils.setFieldValue(at.getJavaField(), o, attVal);
    }

    public void remove(Object object) {
        ensureOpen();

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
            case NOT_MANAGED:
                throw new IllegalArgumentException("Entity " + object
                        + " is not managed and cannot be removed.");
        }
    }

    @Override
    public <T> T find(Class<T> cls, Object primaryKey) {
        final EntityDescriptor d = new EntityDescriptor();
        return find(cls, primaryKey, d);
    }

    @Override
    public <T> T find(Class<T> cls, Object primaryKey, Descriptor descriptor) {
        Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
        Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
        Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

        ensureOpen();
        LOG.trace("Finding instance of {} with identifier {} in context ", cls, primaryKey, descriptor);
        final URI uri = (primaryKey instanceof URI) ? (URI) primaryKey : URI.create(primaryKey.toString());

        return getCurrentPersistenceContext().readObject(cls, uri, descriptor);
    }

    public void flush() {
        ensureOpen();

        LOG.trace("Flushing changes...");
        if (!getTransaction().isActive()) {
            throw new TransactionRequiredException();
        }
        this.getCurrentPersistenceContext().writeUncommittedChanges();
    }

    @Override
    public void refresh(Object entity) {
        ensureOpen();
        Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));

        this.getCurrentPersistenceContext().revertObject(entity);
        new SimpleOneLevelCascadeExplorer() {
            @Override
            protected void runCascadedForEach(Object ox2) {
                refresh(ox2);
            }
        }.start(this, entity, CascadeType.REFRESH);
    }

    public void clear() {
        getCurrentPersistenceContext().clear();
    }

    public void detach(Object entity) {
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
    }

    public boolean contains(Object entity) {
        ensureOpen();
        return getCurrentPersistenceContext().contains(entity);
    }

    public void close() {
        ensureOpen();
        removeCurrentPersistenceContext();
        open = false;
    }

    public boolean isOpen() {
        return open;
    }

    @Override
    public EntityTransaction getTransaction() {
        return transaction.getTransaction();
    }

    public EntityManagerFactoryImpl getEntityManagerFactory() {
        return emf;
    }

    public Metamodel getMetamodel() {
        return emf.getMetamodel();
    }

    public boolean isLoaded(final Object object, final String attributeName) {
        // TODO
        return false;
    }

    @Override
    public Query createQuery(String qlString) {
        return getCurrentPersistenceContext().createQuery(qlString);
    }

    @Override
    public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass) {
        return getCurrentPersistenceContext().createQuery(query, resultClass);
    }

    @Override
    public Query createNativeQuery(String sqlString) {
        return getCurrentPersistenceContext().createNativeQuery(sqlString);
    }

    @Override
    public <T> TypedQuery<T> createNativeQuery(String sqlString, Class<T> resultClass) {
        return getCurrentPersistenceContext().createNativeQuery(sqlString, resultClass);
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

    public <T> T unwrap(Class<T> cls) {
        if (cls.equals(this.getClass())) {
            return cls.cast(this);
        }

        throw new OWLPersistenceException();
    }

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
    public void removeCurrentPersistenceContext() {
        if (persistenceContext != null && persistenceContext.isActive()) {
            persistenceContext.release();
        }
        this.persistenceContext = null;
    }

    public void transactionStarted(EntityTransaction t) {
        this.serverSession.transactionStarted(t, this);
    }

    public void transactionFinished(EntityTransaction t) {
        this.serverSession.transactionFinished(t);
    }

    /**
     * Since we support only EntityTransactions, we set the TransactionWrapper to EntityTransactionWrapper. In the
     * future, if JTA transactions are supported, JTATransactionWrapper should be set instead of the
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
