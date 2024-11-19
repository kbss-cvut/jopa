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

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxyFactory;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.util.CloneConfiguration;
import cz.cvut.kbss.jopa.sessions.util.CloneRegistrationDescriptor;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * Builds clones used in transactions for tracking changes.
 */
public class CloneBuilder {

    private static final Logger LOG = LoggerFactory.getLogger(CloneBuilder.class);

    // Contains entities that are already cloned, so that we don't clone them again
    private final RepositoryMap visitedEntities;

    private final Builders builders;

    private final LazyLoadingProxyFactory lazyLoaderFactory;

    private final AbstractUnitOfWork uow;

    public CloneBuilder(AbstractUnitOfWork uow) {
        this.uow = uow;
        this.visitedEntities = new RepositoryMap();
        this.builders = new Builders();
        this.lazyLoaderFactory = new LazyLoadingProxyFactory(uow);
    }

    /**
     * Builds clone of the given object.
     *
     * @param original           Object
     * @param cloneConfiguration Configuration for the cloning process
     * @return The clone
     * @throws NullPointerException If {@code original} is {@code null}
     */
    public Object buildClone(Object original, CloneConfiguration cloneConfiguration) {
        Objects.requireNonNull(original);
        Objects.requireNonNull(cloneConfiguration);
        if (LOG.isTraceEnabled()) {
            // Normally this is a bad practice, but since stringify could be quite costly, we want to avoid it if possible
            LOG.trace("Cloning object {}.", uow.stringify(original));
        }
        return buildCloneImpl(null, null, original, cloneConfiguration);
    }

    /**
     * Builds clone of the given object.
     * <p>
     * This method differs from {@link #buildClone(Object, CloneConfiguration)} in that it accepts another argument
     * which represents the owner of the built clone. This is useful in situations when we are cloning attributes
     * directly, e.g. when lazily loading a field value.
     *
     * @param cloneOwner  The owner of the created clone
     * @param clonedField The field whose value is being cloned
     * @param original    The original to clone
     * @param descriptor  Entity descriptor
     * @return The clone
     * @throws NullPointerException If {@code cloneOwner}, {@code original} or {@code contextUri} is {@code null}
     */
    public Object buildClone(Object cloneOwner, Field clonedField, Object original, Descriptor descriptor) {
        if (cloneOwner == null || original == null || descriptor == null) {
            throw new NullPointerException();
        }
        if (LOG.isTraceEnabled()) {
            // Normally this is a bad practice, but since stringify could be quite costly, we want to avoid it if possible
            LOG.trace("Cloning object {} with owner {}", uow.stringify(original), uow.stringify(cloneOwner));
        }
        return buildCloneImpl(cloneOwner, clonedField, original, CloneConfiguration.withDescriptor(descriptor));
    }

    private Object buildCloneImpl(Object cloneOwner, Field clonedField, Object original,
                                  CloneConfiguration cloneConfiguration) {
        assert original != null;
        if (isOriginalInUoW(original)) {
            return uow.getCloneForOriginal(original);
        }
        final Class<?> cls = original.getClass();
        final boolean managed = isTypeManaged(cls);
        final boolean hasBuilder = instanceHasBuilder(original);
        final Descriptor descriptor = cloneConfiguration.getDescriptor();
        if (managed) {
            final Object visitedClone = getVisitedEntity(descriptor, original);
            if (visitedClone != null) {
                return visitedClone;
            }
        }
        final AbstractInstanceBuilder builder = getInstanceBuilder(original);
        Object clone = builder.buildClone(cloneOwner, clonedField, original, cloneConfiguration);
        if (managed) {
            // Register visited object before populating attributes to prevent infinite cloning cycles
            putVisitedEntity(descriptor, original, clone);
            final LoadStateDescriptor<Object> loadState = cloneLoadStateDescriptor(original, clone);
            uow.getLoadStateRegistry().put(clone, loadState);
        }
        if (!builder.populatesAttributes() && (managed || hasBuilder)) {
            populateAttributes(original, clone, cloneConfiguration);
        }
        return clone;
    }

    private LoadStateDescriptor<Object> cloneLoadStateDescriptor(Object original, Object clone) {
        if (!uow.getLoadStateRegistry().contains(original)) {
            uow.getLoadStateRegistry().put(original, LoadStateDescriptorFactory.createAllUnknown(original, (EntityType<? super Object>) getMetamodel().entity(original.getClass())));
        }
        final LoadStateDescriptor<Object> origLoadState = uow.getLoadStateRegistry().get(original);
        return LoadStateDescriptorFactory.createCopy(clone, origLoadState);
    }

    /**
     * Clone all the attributes of the original and set the clone values. This also means cloning any relationships and
     * their targets.
     */
    private void populateAttributes(Object original, Object clone, CloneConfiguration configuration) {
        final Class<?> originalClass = original.getClass();
        final EntityType<?> et = getMetamodel().entity(originalClass);
        final LoadStateDescriptor<Object> loadState = uow.getLoadStateRegistry().get(clone);
        // Ensure the identifier is cloned before any other attributes
        // This prevents problems where circular references between entities lead to clones being registered with null identifier
        cloneIdentifier(original, clone, et);
        for (FieldSpecification<?, ?> fs : et.getFieldSpecifications()) {
            if (fs == et.getIdentifier()) {
                continue;   // Already cloned
            }
            final Field f = fs.getJavaField();
            final Object origVal = EntityPropertiesUtils.getFieldValue(f, original);
            Object clonedValue;
            if (loadState.isLoaded(fs) == LoadState.NOT_LOADED) {
                clonedValue = lazyLoaderFactory.createProxy(clone, (FieldSpecification<? super Object, ?>) fs);
            } else if (origVal == null) {
                 continue;
            } else {
                final Class<?> origValueClass = origVal.getClass();
                if (IndirectWrapperHelper.requiresIndirectWrapper(origVal)) {
                    final Descriptor fieldDescriptor = getFieldDescriptor(f, originalClass, configuration.getDescriptor());
                    // Collection or Map
                    clonedValue = getInstanceBuilder(origVal).buildClone(clone, f, origVal,
                            CloneConfiguration.withDescriptor(fieldDescriptor)
                                              .forPersistenceContext(configuration.isForPersistenceContext())
                                              .addPostRegisterHandlers(configuration.getPostRegister()));
                } else if(isTypeManaged(origValueClass) || instanceHasBuilder(origVal)) {
                    // Otherwise, we have a relationship, and we need to clone its target as well
                    if (isOriginalInUoW(origVal)) {
                        // If the reference is already managed
                        clonedValue = uow.getCloneForOriginal(origVal);
                    } else {
                        if (isTypeManaged(origValueClass)) {
                            final Descriptor fieldDescriptor =
                                    getFieldDescriptor(f, originalClass, configuration.getDescriptor());
                            clonedValue = getVisitedEntity(configuration.getDescriptor(), origVal);
                            if (clonedValue == null) {
                                clonedValue = uow.registerExistingObject(origVal, new CloneRegistrationDescriptor(fieldDescriptor).postCloneHandlers(configuration.getPostRegister()));
                            }
                        } else {
                            clonedValue = buildClone(origVal, configuration);
                        }
                    }
                } else {
                    // We assume that the value is immutable
                    clonedValue = origVal;
                }
            }
            EntityPropertiesUtils.setFieldValue(f, clone, clonedValue);
        }
    }

    private static void cloneIdentifier(Object original, Object clone, EntityType<?> et) {
        final Identifier<?, ?> identifier = et.getIdentifier();
        final Object idValue = EntityPropertiesUtils.getFieldValue(identifier.getJavaField(), original);
        EntityPropertiesUtils.setFieldValue(identifier.getJavaField(), clone, idValue);
    }

    private Descriptor getFieldDescriptor(Field field, Class<?> entityClass, Descriptor entityDescriptor) {
        final EntityType<?> et = getMetamodel().entity(entityClass);
        final FieldSpecification<?, ?> fieldSpec = et.getFieldSpecification(field.getName());
        return entityDescriptor.getAttributeDescriptor(fieldSpec);
    }

    /**
     * Merges the changes on clone into the original object.
     *
     * @param changeSet Contains changes to merge
     */
    public void mergeChanges(ObjectChangeSet changeSet) {
        final Object original = changeSet.getOriginal();
        final LoadStateDescriptor<?> loadStateDescriptor = uow.getLoadStateRegistry().get(original);
        try {
            for (ChangeRecord change : changeSet.getChanges()) {
                Field f = change.getAttribute().getJavaField();

                Object origVal = EntityPropertiesUtils.getFieldValue(f, original);
                Object newVal = change.getNewValue();

                if(newVal == null) {
                    EntityPropertiesUtils.setFieldValue(f, original, null);
                }

                if(isTypeManaged(f.getType()) || instanceHasBuilder(newVal)) {
                    getInstanceBuilder(newVal).mergeChanges(f, original, origVal, newVal);
                    continue;
                } else {
                    EntityPropertiesUtils.setFieldValue(f, original, newVal);
                }
                loadStateDescriptor.setLoaded((FieldSpecification<? super Object, ?>) change.getAttribute(), LoadState.LOADED);
            }
        } catch (SecurityException e) {
            throw new OWLPersistenceException(e);
        }
    }

    private Object getVisitedEntity(Descriptor descriptor, Object original) {
        assert descriptor != null;
        assert original != null;
        return visitedEntities.get(descriptor, original);
    }

    private void putVisitedEntity(Descriptor descriptor, Object original, Object clone) {
        assert descriptor != null;
        visitedEntities.add(descriptor, original, clone);
    }

    AbstractInstanceBuilder getInstanceBuilder(Object toClone) {
        return builders.getBuilder(toClone);
    }

    boolean instanceHasBuilder(Object toClone) { return builders.hasBuilder(toClone); }

    boolean isTypeManaged(Class<?> cls) {
        return uow.isEntityType(cls);
    }

    boolean isOriginalInUoW(Object original) {
        return uow.containsOriginal(original);
    }

    Object getOriginal(Object clone) {
        return uow.getOriginal(clone);
    }

    Metamodel getMetamodel() {
        return uow.getMetamodel();
    }

    /**
     * Resets the clone builder.
     * <p>
     * Especially resets the visited objects cache to make sure all the clones are built from scratch and are not
     * affected by the previously built ones.
     */
    public void reset() {
        visitedEntities.clear();
    }

    /**
     * Removes the specified instance from the clone builder's visited entities cache.
     *
     * @param instance   The instance to remove (original object).
     * @param descriptor Instance descriptor
     */
    public void removeVisited(Object instance, Descriptor descriptor) {
        visitedEntities.remove(descriptor, instance);
    }


    private final class Builders {
        private final AbstractInstanceBuilder defaultBuilder;
        private final AbstractInstanceBuilder managedInstanceBuilder;
        private final AbstractInstanceBuilder dateBuilder;
        private final AbstractInstanceBuilder multilingualStringBuilder;
        // Lists and Sets
        private final AbstractInstanceBuilder collectionBuilder;
        private final AbstractInstanceBuilder mapBuilder;

        private Builders() {
            this.defaultBuilder = new DefaultInstanceBuilder(CloneBuilder.this, uow);
            this.managedInstanceBuilder = new ManagedInstanceBuilder(CloneBuilder.this, uow);
            this.dateBuilder = new DateInstanceBuilder(CloneBuilder.this, uow);
            this.multilingualStringBuilder = new MultilingualStringInstanceBuilder(CloneBuilder.this, uow);
            this.mapBuilder = new MapInstanceBuilder(CloneBuilder.this, uow);
            this.collectionBuilder = new CollectionInstanceBuilder(CloneBuilder.this, uow);
        }

        private Optional<AbstractInstanceBuilder> getAbstractInstanceBuilder(Object toClone) {
            if(toClone instanceof Date) {
                return Optional.of(dateBuilder);
            } else if(toClone instanceof MultilingualString) {
                return Optional.of(multilingualStringBuilder);
            } else if(toClone instanceof Map) {
                return Optional.of(mapBuilder);
            } else if(toClone instanceof Collection<?>) {
                return Optional.of(collectionBuilder);
            }

            return Optional.empty();
        }

        private boolean hasBuilder(Object toClone) {
            return getAbstractInstanceBuilder(toClone).isPresent();
        }

        private AbstractInstanceBuilder getBuilder(Object toClone) {
            return getAbstractInstanceBuilder(toClone)
                    .orElseGet(() -> {
                        if (isTypeManaged(toClone.getClass())) {
                            return managedInstanceBuilder;
                        }

                        return defaultBuilder;
                    });
        }
    }
}
