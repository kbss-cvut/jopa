package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectWrapper;
import cz.cvut.kbss.jopa.exceptions.EntityNotFoundException;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.Manageable;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.change.ChangeSetFactory;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.descriptor.InstanceDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.validator.AttributeModificationValidator;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Objects;

import static cz.cvut.kbss.jopa.utils.EntityPropertiesUtils.getValueAsURI;

public class ChangeTrackingUnitOfWork extends AbstractUnitOfWork{

    private final IndirectWrapperHelper indirectWrapperHelper;

    public ChangeTrackingUnitOfWork(AbstractSession parent, Configuration configuration) {
        super(parent, configuration);
        this.indirectWrapperHelper = new IndirectWrapperHelper(this);
    }

    @Override
    protected <T> T readObjectInternal(Class<T> cls, Object identifier, Descriptor descriptor) {
        final T clone = super.readObjectInternal(cls, identifier, descriptor);
        if (clone != null) {
            checkForIndirectObjects(clone);
        }
        return clone;
    }

    /**
     * Check if the specified entity contains a collection. If so, replace it with its indirect representation so that
     * changes in that collection can be tracked.
     *
     * @param entity The entity to check
     */
    private void checkForIndirectObjects(Object entity) {
        assert entity != null;
        final EntityType<?> et = entityType(entity.getClass());
        for (FieldSpecification<?, ?> fieldSpec : et.getFieldSpecifications()) {
            setIndirectObjectIfPresent(entity, fieldSpec.getJavaField());
        }
    }

    /**
     * Create and set indirect collection on the specified entity field.
     * <p>
     * If the specified field is of Collection type, and it is not already an indirect collection, create new one and set
     * it as the value of the specified field on the specified entity.
     *
     * @param entity The entity collection will be set on
     * @param field  The field to set
     * @throws IllegalArgumentException Reflection
     */
    private void setIndirectObjectIfPresent(Object entity, Field field) {
        assert entity != null;
        assert field != null;

        final Object value = EntityPropertiesUtils.getFieldValue(field, entity);
        if (value instanceof IndirectWrapper) {
            return;
        }
        if (IndirectWrapperHelper.requiresIndirectWrapper(value)) {
            EntityPropertiesUtils.setFieldValue(field, entity, indirectWrapperHelper.createIndirectWrapper(value, entity, field));
        }
    }

    @Override
    public <T> T getReference(Class<T> cls, Object identifier, Descriptor descriptor) {
        Objects.requireNonNull(cls);
        Objects.requireNonNull(identifier);
        Objects.requireNonNull(descriptor);

        final T managedResult = readManagedObject(cls, identifier, descriptor);
        if (managedResult != null) {
            return managedResult;
        }
        final T result = storage.getReference(new LoadingParameters<>(cls, getValueAsURI(identifier), descriptor));
        if (result == null) {
            return null;
        }
        final T clone = (T) cloneBuilder.buildReferenceClone(result, new CloneConfiguration(descriptor, true));
        instanceDescriptors.put(clone, InstanceDescriptorFactory.createNotLoaded(result, entityType(cls)));
        attachPersistenceContextToEntity(clone);
        registerEntityWithOntologyContext(clone, descriptor);
        if (getLiveObjectCache().contains(cls, identifier, descriptor)) {
            cloneToOriginals.put(clone, getLiveObjectCache().get(cls, identifier, descriptor));
        } else {
            cloneToOriginals.put(clone, null);
        }
        keysToClones.put(identifier, clone);
        return clone;
    }

    /**
     * Creates an indirect collection, which wraps the specified collection instance and propagates changes to the
     * persistence context.
     *
     * @param collection Collection to be proxied
     * @param owner      Collection owner instance
     * @param field      Field filled with the collection
     * @return Indirect collection
     */
    public Object createIndirectCollection(Object collection, Object owner, Field field) {
        return indirectWrapperHelper.createIndirectWrapper(collection, owner, field);
    }

    /**
     * If there are any changes, commit them to the ontology.
     */
    void commitToStorage() {
        if (this.hasNew || this.hasChanges || this.hasDeleted) {
            persistNewObjects();
            calculateChanges();
        }
        validateIntegrityConstraints();
        storage.commit();
    }

    @Override
    protected void detachAllManagedInstances() {
        cloneMapping.forEach(instance -> {
            removeIndirectWrappers(instance);
            deregisterEntityFromPersistenceContext(instance);
        });
        newObjectsCloneToOriginal.keySet().forEach(this::removeIndirectWrappers);
    }

    /**
     * Removes {@link IndirectWrapper} instances from the specified entity (if present).
     *
     * @param entity The entity to remove indirect wrappers from
     */
    private void removeIndirectWrappers(Object entity) {
        assert entity != null;
        final EntityType<?> et = entityType(entity.getClass());
        for (FieldSpecification<?, ?> fs : et.getFieldSpecifications()) {
            final Object value = EntityPropertiesUtils.getFieldValue(fs.getJavaField(), entity);
            if (value instanceof IndirectWrapper indirectWrapper) {
                EntityPropertiesUtils.setFieldValue(fs.getJavaField(), entity, indirectWrapper.unwrap());
            }
        }
    }

    @Override
    void registerClone(Object clone, Object original, Descriptor descriptor) {
        super.registerClone(clone, original, descriptor);
        attachPersistenceContextToEntity(clone);
    }

    private void attachPersistenceContextToEntity(Object entity) {
        if (isInCommit()) {
            return;
        }
        assert entity instanceof Manageable;
        ((Manageable) entity).setPersistenceContext(this);
    }

    private static void deregisterEntityFromPersistenceContext(Object entity) {
        if (!(entity instanceof Manageable)) {
            return;
        }
        ((Manageable) entity).setPersistenceContext(null);
    }

    @Override
    public void attributeChanged(Object entity, Field f) {
        final IdentifiableEntityType<Object> et = entityType((Class<Object>) entity.getClass());
        final FieldSpecification<Object, ?> fieldSpec = et.getFieldSpecification(f.getName());
        attributeChanged(entity, fieldSpec);
    }

    @Override
    public void attributeChanged(Object entity, FieldSpecification<?, ?> fieldSpec) {
        if (!isInTransaction()) {
            throw new IllegalStateException("This unit of work is not in a transaction.");
        }
        final Descriptor descriptor = getDescriptor(entity);
        final IdentifiableEntityType<Object> et = entityType((Class<Object>) entity.getClass());
        final Object original = getOriginal(entity);
        if (fieldSpec.isInferred() && original != null) {
            inferredAttributeChangeValidator.validateChange(entity, getOriginal(entity), (FieldSpecification<? super Object, ?>) fieldSpec, descriptor);
        }
        et.getLifecycleListenerManager().invokePreUpdateCallbacks(entity);
        storage.merge(entity, (FieldSpecification<? super Object, ?>) fieldSpec, descriptor);
        createAndRegisterChangeRecord(entity, fieldSpec, descriptor);
        setHasChanges();
        setIndirectObjectIfPresent(entity, fieldSpec.getJavaField());
        et.getLifecycleListenerManager().invokePostUpdateCallbacks(entity);
        instanceDescriptors.get(entity).setLoaded(fieldSpec, LoadState.LOADED);
    }

    private void createAndRegisterChangeRecord(Object clone, FieldSpecification<?, ?> fieldSpec,
                                               Descriptor descriptor) {
        final Object orig = getOriginal(clone);
        if (orig == null) {
            return;
        }
        final ChangeRecord record = new ChangeRecord(fieldSpec, EntityPropertiesUtils.getFieldValue(fieldSpec.getJavaField(), clone));
        preventCachingIfReferenceIsNotLoaded(record);
        registerChangeRecord(clone, orig, descriptor, record);
    }

    private void registerChangeRecord(Object clone, Object orig, Descriptor descriptor, ChangeRecord record) {
        ObjectChangeSet chSet = uowChangeSet.getExistingObjectChanges(orig);
        if (chSet == null) {
            chSet = ChangeSetFactory.createObjectChangeSet(orig, clone, descriptor);
            uowChangeSet.addObjectChangeSet(chSet);
        }
        chSet.addChangeRecord(record);
    }

    <T> T mergeDetachedInternal(T entity, Descriptor descriptor) {
        assert entity != null;
        final IdentifiableEntityType<T> et = (IdentifiableEntityType<T>) entityType(entity.getClass());
        final URI idUri = EntityPropertiesUtils.getIdentifier(entity, et);

        final T clone = getInstanceForMerge(idUri, et, descriptor);
        try {
            // Merge only the changed attributes
            ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(clone, entity, descriptor);
            // Have to check for inferred attribute changes before the actual merge
            changeCalculator.calculateChanges(chSet);
            chSet = processInferredValueChanges(chSet);
            if (chSet.hasChanges()) {
                et.getLifecycleListenerManager().invokePreUpdateCallbacks(clone);
                final DetachedInstanceMerger merger = new DetachedInstanceMerger(this);
                merger.mergeChangesFromDetachedToManagedInstance(chSet, descriptor);
                for (ChangeRecord record : chSet.getChanges()) {
                    AttributeModificationValidator.verifyCanModify(record.getAttribute());
                    preventCachingIfReferenceIsNotLoaded(record);
                    storage.merge(clone, (FieldSpecification<? super T, ?>) record.getAttribute(), descriptor);
                }
                et.getLifecycleListenerManager().invokePostUpdateCallbacks(clone);
                uowChangeSet.addObjectChangeSet(copyChangeSet(chSet, getOriginal(clone), clone, descriptor));
            }
        } catch (OWLEntityExistsException e) {
            unregisterObject(clone);
            throw e;
        }
        evictAfterMerge(et, idUri, descriptor);
        setHasChanges();
        checkForIndirectObjects(clone);
        return et.getJavaType().cast(clone);
    }

    private <T> T getInstanceForMerge(URI identifier, EntityType<T> et, Descriptor descriptor) {
        if (keysToClones.containsKey(identifier)) {
            return (T) keysToClones.get(identifier);
        }
        final LoadingParameters<T> params = new LoadingParameters<>(et.getJavaType(), identifier, descriptor, true);
        T original = storage.find(params);
        assert original != null;

        return (T) registerExistingObject(original, descriptor);
    }

    private ObjectChangeSet processInferredValueChanges(ObjectChangeSet changeSet) {
        if (getConfiguration().is(JOPAPersistenceProperties.IGNORE_INFERRED_VALUE_REMOVAL_ON_MERGE)) {
            final ObjectChangeSet copy = ChangeSetFactory.createObjectChangeSet(changeSet.getChangedObject(), changeSet.getCloneObject(), changeSet.getEntityDescriptor());
            changeSet.getChanges().stream().filter(chr -> !(chr.getAttribute().isInferred() &&
                    inferredAttributeChangeValidator.isInferredValueRemoval(changeSet.getCloneObject(), changeSet.getChangedObject(),
                            (FieldSpecification) chr.getAttribute(),
                            changeSet.getEntityDescriptor()))).forEach(copy::addChangeRecord);
            return copy;
        } else {
            changeSet.getChanges().stream().filter(chr -> chr.getAttribute().isInferred()).forEach(
                    chr -> inferredAttributeChangeValidator.validateChange(changeSet.getCloneObject(), changeSet.getChangedObject(),
                            (FieldSpecification) chr.getAttribute(),
                            changeSet.getEntityDescriptor()));
            return changeSet;
        }
    }

    private static ObjectChangeSet copyChangeSet(ObjectChangeSet changeSet, Object original, Object clone,
                                                 Descriptor descriptor) {
        final ObjectChangeSet newChangeSet = ChangeSetFactory.createObjectChangeSet(original, clone, descriptor);
        changeSet.getChanges().forEach(newChangeSet::addChangeRecord);
        return newChangeSet;
    }

    private void evictAfterMerge(EntityType<?> et, URI identifier, Descriptor descriptor) {
        if (getLiveObjectCache().contains(et.getJavaType(), identifier, descriptor)) {
            getLiveObjectCache().evict(et.getJavaType(), identifier, descriptor.getSingleContext().orElse(null));
        }
        getMetamodel().getReferringTypes(et.getJavaType()).forEach(getLiveObjectCache()::evict);
    }

    @Override
    public <T> void refreshObject(T object) {
        Objects.requireNonNull(object);
        ensureManaged(object);

        final IdentifiableEntityType<T> et = entityType((Class<T>) object.getClass());
        final URI idUri = EntityPropertiesUtils.getIdentifier(object, et);
        final Descriptor descriptor = getDescriptor(object);

        final LoadingParameters<T> params = new LoadingParameters<>(et.getJavaType(), idUri, descriptor, true);
        params.bypassCache();
        final ConnectionWrapper connection = acquireConnection();
        try {
            uowChangeSet.cancelObjectChanges(getOriginal(object));
            T original = connection.find(params);
            if (original == null) {
                throw new EntityNotFoundException("Entity " + object + " no longer exists in the repository.");
            }
            T source = (T) cloneBuilder.buildClone(original, new CloneConfiguration(descriptor, false));
            final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(source, object, descriptor);
            changeCalculator.calculateChanges(chSet);
            new RefreshInstanceMerger(indirectWrapperHelper).mergeChanges(chSet);
            revertTransactionalChanges(object, descriptor, chSet);
            registerClone(object, original, descriptor);
            et.getLifecycleListenerManager().invokePostLoadCallbacks(object);
        } finally {
            connection.close();
        }
    }

    private <T> void revertTransactionalChanges(T object, Descriptor descriptor, ObjectChangeSet chSet) {
        for (ChangeRecord change : chSet.getChanges()) {
            storage.merge(object, (FieldSpecification<? super T, ?>) change.getAttribute(), descriptor.getAttributeDescriptor(change.getAttribute()));
        }
    }

    @Override
    public void registerNewObject(Object entity, Descriptor descriptor) {
        super.registerNewObject(entity, descriptor);
        checkForIndirectObjects(entity);
    }

    @Override
    public void unregisterObject(Object object) {
        super.unregisterObject(object);
        removeIndirectWrappers(object);
        deregisterEntityFromPersistenceContext(object);
    }
}
