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

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.utils.Configuration;

import java.lang.reflect.Field;
import java.net.URI;

/**
 * Read-only UoW which clones all entities before returning them to the application.
 * <p>
 * Cloning entities allows using second-level cache which brings better performance than {@link ReadOnlyUnitOfWork}
 * which does not clone entities but also skips second-level cache (unless entity is already there in which case it
 * behaves exactly like this class).
 */
public class CloningReadOnlyUnitOfWork extends AbstractUnitOfWork {

    CloningReadOnlyUnitOfWork(AbstractSession parent, Configuration configuration) {
        super(parent, configuration);
    }

    @Override
    void detachAllManagedInstances() {
        cloneMapping.forEach(this::removeIndirectWrappersAndProxies);
    }

    /**
     * {@code ReadOnlyUnitOfWork} commits nothing. The persistence context is cleared.
     */
    @Override
    public void commit() {
        LOG.trace("Read-only UnitOfWork commit started. Nothing is commited to a database.");
        if (!isActive()) {
            throw new IllegalStateException("Cannot commit inactive Unit of Work!");
        }
        commitToStorage();
        clear();
        LOG.trace("UnitOfWork commit finished.");
    }

    @Override
    public void commitToStorage() {
        storage.commit();
    }

    @Override
    public Object createIndirectCollection(Object collection, Object owner, Field field) {
        // Do not create any special kind of collection, just return the argument
        return collection;
    }

    @Override
    public boolean isReadOnly() {
        return true;
    }


    @Override
    public boolean hasChanges() throws UnsupportedOperationException {
        return false;
    }

    @Override
    public boolean isFlushingChanges() {
        return false;
    }

    /// ///////////////////////////////////THESE METHODS SHOULD NOT BE SUPPORTED///////////////////////////////////////

    private static void throwUnsupportedOperationException() {
        throw new UnsupportedOperationException("Method not supported.");
    }

    /**
     * Method not supported.
     *
     * @throws UnsupportedOperationException Method not supported.
     */
    @Override
    public void removeObjectFromCache(Object toRemove, URI context) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }

    /**
     * Method not supported.
     *
     * @throws UnsupportedOperationException Method not supported.
     */
    @Override
    void preventCachingIfReferenceIsNotLoaded(ChangeRecord changeRecord) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }

    /**
     * Method not supported.
     *
     * @throws UnsupportedOperationException Method not supported.
     */
    @Override
    public <T> T mergeDetached(T entity, Descriptor descriptor) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
        return null;
    }

    /**
     * Method not supported.
     *
     * @throws UnsupportedOperationException Method not supported.
     */
    @Override
    public <T> T mergeDetachedInternal(T entity, Descriptor descriptor) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
        return null;
    }

    /**
     * Method not supported.
     *
     * @throws UnsupportedOperationException Method not supported.
     */
    @Override
    protected <T> T getInstanceForMerge(URI identifier, EntityType<T> et,
                                        Descriptor descriptor) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
        return null;
    }

    /**
     * Method not supported.
     *
     * @throws UnsupportedOperationException Method not supported.
     */
    @Override
    protected void evictAfterMerge(EntityType<?> et, URI identifier,
                                   Descriptor descriptor) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }

    /**
     * Method not supported.
     *
     * @throws UnsupportedOperationException Method not supported.
     */
    @Override
    public <T> void refreshObject(T object) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }

    /**
     * Method is not supported.
     *
     * @throws UnsupportedOperationException Always thrown
     */
    @Override
    protected ObjectChangeSet processInferredValueChanges(
            ObjectChangeSet changeSet) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
        return null;
    }

    /**
     * Method is not supported.
     *
     * @throws UnsupportedOperationException Always thrown
     */
    @Override
    void validateIntegrityConstraints() throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }

    /**
     * Method is not supported.
     *
     * @throws UnsupportedOperationException Always thrown
     */
    @Override
    void calculateChanges() throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }

    /**
     * Method is not supported.
     *
     * @throws UnsupportedOperationException Always thrown
     */
    @Override
    void persistNewObjects() throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }

    /**
     * Method is not supported.
     *
     * @throws UnsupportedOperationException Always thrown
     */
    @Override
    public void registerOriginalForNewClone(Object clone, Object original) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }

    /**
     * Method is not supported.
     *
     * @throws UnsupportedOperationException Always thrown
     */
    @Override
    public void writeUncommittedChanges() throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }

    /**
     * Method is not supported.
     *
     * @throws UnsupportedOperationException Always thrown
     */
    @Override
    void setHasChanges() throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }

    /**
     * Method is not supported.
     *
     * @throws UnsupportedOperationException Always thrown
     */
    @Override
    public void restoreRemovedObject(Object entity) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }

    /**
     * Method is not supported.
     *
     * @throws UnsupportedOperationException Always thrown
     */
    @Override
    public void attributeChanged(Object entity, Field f) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }

    /**
     * Method is not supported.
     *
     * @throws UnsupportedOperationException Always thrown
     */
    @Override
    public void attributeChanged(Object entity,
                                 FieldSpecification<?, ?> fieldSpec) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }

    /**
     * Method is not supported.
     *
     * @throws UnsupportedOperationException Always thrown
     */
    @Override
    protected void markCloneForDeletion(Object entity, Object identifier) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }


    /**
     * Method is not supported.
     *
     * @throws UnsupportedOperationException Always thrown
     */
    @Override
    public void registerNewObject(Object entity, Descriptor descriptor) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }

    /**
     * Method is not supported.
     *
     * @throws UnsupportedOperationException Always thrown
     */
    @Override
    public void removeObject(Object object) throws UnsupportedOperationException {
        throwUnsupportedOperationException();
    }
}
