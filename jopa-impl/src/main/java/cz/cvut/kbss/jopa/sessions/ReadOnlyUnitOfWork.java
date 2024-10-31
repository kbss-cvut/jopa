/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.EntityState;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryFactory;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.change.UnitOfWorkChangeSet;
import cz.cvut.kbss.jopa.sessions.util.CloneRegistrationDescriptor;
import cz.cvut.kbss.jopa.sessions.util.LoadStateDescriptorRegistry;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.sessions.cache.CacheManager;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;


public class ReadOnlyUnitOfWork extends AbstractUnitOfWork {

    ReadOnlyUnitOfWork(AbstractSession parent, Configuration configuration) {
        super(parent, configuration);
    }

    @Override
    protected ConnectionWrapper acquireConnection() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void release() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    /**
     * Detaches all managed entities from this persistence context.
     */
    public void detachAllManagedInstances() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public CacheManager getLiveObjectCache() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public boolean isActive() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void begin() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void commit() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    /**
     * Commit this Unit of Work.
     */
    private void commitUnitOfWork() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    void removeLazyLoadingProxies(Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    /**
     * If there are any changes, commit them to the ontology.
     */
    public void commitToStorage() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    /**
     * Merge the changes from this Unit of Work's change set into the server session.
     */
    private void mergeChangesIntoParent() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    private void evictPossiblyUpdatedReferencesFromCache() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    /**
     * Cleans up after the commit.
     */
    private void postCommit() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void rollback() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public boolean contains(Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public <T> T readObject(Class<T> cls, Object identifier, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    protected <T> T readObjectInternal(Class<T> cls, Object identifier, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    <T> T readManagedObject(Class<T> cls, Object identifier, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    private boolean isInRepository(Descriptor descriptor, Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    private <T> T getManagedClone(Class<T> cls, Object identifier, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public <T> T getReference(Class<T> cls, Object identifier, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public <T> T readObjectWithoutRegistration(Class<T> cls, Object identifier, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public EntityState getState(Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public EntityState getState(Object entity, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public boolean isObjectNew(Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public boolean isObjectManaged(Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    private boolean isManagedReference(Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public <T> T mergeDetached(T entity, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    Object getIdentifier(Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    private boolean isSameType(Object id, Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    /**
     * Merges the specified detached entity into this persistence context.
     *
     * @param entity     Entity to merge
     * @param descriptor Descriptor of the merged entity
     * @param <T>        Entity type
     * @return Managed instance
     */
    public <T> T mergeDetachedInternal(T entity, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public Object registerExistingObject(Object entity, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public Object registerExistingObject(Object entity, CloneRegistrationDescriptor registrationDescriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    void registerClone(Object clone, Object original, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    protected <T> IdentifiableEntityType<T> entityType(Class<T> cls) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    /**
     * This method calculates the changes that were to the registered entities and adds these changes into the given
     * change set for future commit to the ontology.
     */
    void calculateChanges() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    /**
     * Create object change sets for the new objects and adds them into our UnitOfWorkChangeSet.
     */
    private void calculateNewObjects(UnitOfWorkChangeSet changeSet) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    private void calculateDeletedObjects(final UnitOfWorkChangeSet changeSet) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    void persistNewObjects() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    void validateIntegrityConstraints() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    /**
     * Tries to find the original object for the given clone. It searches the existing objects, new objects and deleted
     * objects.
     *
     * @param clone Object
     * @return The original object for the given clone
     */
    public Object getOriginal(Object clone) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    /**
     * Registers the specified original for the specified clone, assuming the clone is a new object.
     * <p>
     * This method must be called during commit when new objects are persisted so that they can be referenced by other
     * objects.
     *
     * @param clone    Already registered clone
     * @param original Original to register
     */
    public void registerOriginalForNewClone(Object clone, Object original) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    /**
     * Gets managed original with the specified identifier or {@code null} if there is none matching.
     * <p>
     * Descriptor is used to check repository context validity.
     *
     * @param cls        Return type of the original
     * @param identifier Instance identifier
     * @param descriptor Repository descriptor
     * @return Original object managed by this UoW or {@code null} if this UoW doesn't contain a matching instance
     */
    public <T> T getManagedOriginal(Class<T> cls, Object identifier, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    /**
     * Check if this UnitOfWork contains this original entity. This method is used by the CloneBuilder so it does not
     * have to clone already managed referenced objects.
     *
     * @param entity The original entity.
     * @return True if the original is managed in this UnitOfWork.
     */
    boolean containsOriginal(Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    /**
     * Finds clone of the specified original.
     *
     * @param original The original object whose clone we are looking for
     * @return The clone or null, if there is none
     */
    public Object getCloneForOriginal(Object original) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    public boolean hasChanges() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    void setHasChanges() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    void preventCachingIfReferenceIsNotLoaded(ChangeRecord changeRecord) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    protected ObjectChangeSet processInferredValueChanges(ObjectChangeSet changeSet) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    protected <T> T getInstanceForMerge(URI identifier, EntityType<T> et, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    protected void evictAfterMerge(EntityType<?> et, URI identifier, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    protected static ObjectChangeSet copyChangeSet(ObjectChangeSet changeSet, Object original, Object clone,
                                                   Descriptor descriptor) {

        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public <T> void refreshObject(T object) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    private <T> void revertTransactionalChanges(T object, Descriptor descriptor, ObjectChangeSet chSet) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void registerNewObject(Object entity, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void removeObject(Object object) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    private Object initEntityIdentifier(Object entity, EntityType<Object> et) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    private void verifyCanPersist(Object id, Object instance, EntityType<?> et, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    private boolean isIndividualManaged(Object identifier, Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }


    <T> void ensureManaged(T object) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void restoreRemovedObject(Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void unregisterObject(Object object) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    private void unregisterEntityFromOntologyContext(Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void writeUncommittedChanges() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public boolean isEntityType(Class<?> cls) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public boolean isInTransaction() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public boolean isFlushingChanges() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public <T> Object loadEntityField(T entity, FieldSpecification<? super T, ?> fieldSpec) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    /**
     * Gets basic object info for logging.
     * <p>
     * This works around using {@link Object#toString()} for entities, which could inadvertently trigger lazy field
     * fetching and cause an infinite field loading loop.
     *
     * @param object Object to stringify
     * @return String info about the specified object
     */
    public String stringify(Object object) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    private <T> Descriptor getFieldDescriptor(T entity, Field field, Descriptor entityDescriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    private <T> Object cloneLoadedFieldValue(T entity, Field field, final Descriptor fieldDescriptor,
                                             final Object fieldValueOrig) {

        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void putObjectIntoCache(Object identifier, Object entity, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void removeObjectFromCache(Object toRemove, URI context) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public boolean isConsistent(URI context) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public List<URI> getContexts() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public <T> boolean isInferred(T entity, FieldSpecification<? super T, ?> attribute, Object value) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void attributeChanged(Object entity, Field f) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void attributeChanged(Object entity, FieldSpecification<?, ?> fieldSpec) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public Object createIndirectCollection(Object collection, Object owner, Field field) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public LoadState isLoaded(Object entity, String attributeName) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public LoadState isLoaded(Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    public SparqlQueryFactory sparqlQueryFactory() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public void setReadOnly(boolean readOnly) throws Exception {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public boolean isReadOnly() throws Exception {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    void registerEntityWithOntologyContext(Object entity, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    Descriptor getDescriptor(Object entity) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    public LoadStateDescriptorRegistry getLoadStateRegistry() {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    @Override
    public <T> T unwrap(Class<T> cls) {
        throw new UnsupportedOperationException("Method not implemented.");
    }

    protected void markCloneForDeletion(Object entity, Object identifier) {
        throw new UnsupportedOperationException("Method not implemented.");
    }
}
