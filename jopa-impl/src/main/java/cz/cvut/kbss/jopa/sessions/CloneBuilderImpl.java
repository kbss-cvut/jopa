/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.LangString;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.net.URL;
import java.time.*;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CloneBuilderImpl implements CloneBuilder {

    private static final Logger LOG = LoggerFactory.getLogger(CloneBuilderImpl.class);

    private static final Set<Class<?>> IMMUTABLE_TYPES = getImmutableTypes();

    // Contains entities that are already cloned, so that we don't clone them again
    private final RepositoryMap visitedEntities;

    private final Builders builders;

    private final UnitOfWorkImpl uow;

    public CloneBuilderImpl(UnitOfWorkImpl uow) {
        this.uow = uow;
        this.visitedEntities = new RepositoryMap();
        this.builders = new Builders();
    }

    @Override
    public Object buildClone(Object original, CloneConfiguration cloneConfiguration) {
        Objects.requireNonNull(original);
        Objects.requireNonNull(cloneConfiguration);
        if (LOG.isTraceEnabled()) {
            // Normally this is a bad practice, but since stringify could be quite costly, we want to avoid it if possible
            LOG.trace("Cloning object {}.", stringify(original));
        }
        return buildCloneImpl(null, null, original, cloneConfiguration);
    }

    @Override
    public Object buildClone(Object cloneOwner, Field clonedField, Object original, Descriptor descriptor) {
        if (cloneOwner == null || original == null || descriptor == null) {
            throw new NullPointerException();
        }
        if (LOG.isTraceEnabled()) {
            // Normally this is a bad practice, but since stringify could be quite costly, we want to avoid it if possible
            LOG.trace("Cloning object {} with owner {}", stringify(original), stringify(cloneOwner));
        }
        return buildCloneImpl(cloneOwner, clonedField, original, new CloneConfiguration(descriptor));
    }

    private Object buildCloneImpl(Object cloneOwner, Field clonedField, Object original,
                                  CloneConfiguration cloneConfiguration) {
        assert original != null;
        if (isOriginalInUoW(original)) {
            return uow.getCloneForOriginal(original);
        }
        final Class<?> cls = original.getClass();
        final boolean managed = isTypeManaged(cls);
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
            // Register visited object before populating attributes to prevent endless cloning cycles
            putVisitedEntity(descriptor, original, clone);
        }
        if (!builder.populatesAttributes() && !isImmutable(cls)) {
            populateAttributes(original, clone, cloneConfiguration);
        }
        return clone;
    }

    /**
     * Clone all the attributes of the original and set the clone values. This also means cloning any relationships and
     * their targets.
     */
    private void populateAttributes(Object original, Object clone, CloneConfiguration configuration) {
        final Class<?> originalClass = original.getClass();
        final EntityType<?> et = getMetamodel().entity(originalClass);
        // Ensure the identifier is cloned before any other attributes
        // This prevents problems where circular references between entities lead to clones being registered with null identifier
        cloneIdentifier(original, clone, et);
        for (FieldSpecification<?, ?> fs : et.getFieldSpecifications()) {
            if (fs == et.getIdentifier()) {
                continue;   // Already cloned
            }
            final Field f = fs.getJavaField();
            final Object origVal = EntityPropertiesUtils.getFieldValue(f, original);
            if (origVal == null) {
                continue;
            }
            final Class<?> origValueClass = origVal.getClass();
            Object clonedValue;
            if (isImmutable(origValueClass)) {
                // The field is an immutable type
                clonedValue = origVal;
            } else if (IndirectWrapperHelper.requiresIndirectWrapper(origVal)) {
                final Descriptor fieldDescriptor = getFieldDescriptor(f, originalClass, configuration.getDescriptor());
                // Collection or Map
                clonedValue = getInstanceBuilder(origVal).buildClone(clone, f, origVal,
                                                                     new CloneConfiguration(fieldDescriptor,
                                                                                            configuration.getPostRegister()));
            } else {
                // Otherwise we have a relationship and we need to clone its target as well
                if (isOriginalInUoW(origVal)) {
                    // If the reference is already managed
                    clonedValue = uow.getCloneForOriginal(origVal);
                } else {
                    if (isTypeManaged(origValueClass)) {
                        final Descriptor fieldDescriptor =
                                getFieldDescriptor(f, originalClass, configuration.getDescriptor());
                        clonedValue = getVisitedEntity(configuration.getDescriptor(), origVal);
                        if (clonedValue == null) {
                            clonedValue = uow.registerExistingObject(origVal, fieldDescriptor,
                                                                     configuration.getPostRegister());
                        }
                    } else {
                        clonedValue = buildClone(origVal, configuration);
                    }
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
     * Check if the given class is an immutable type.
     * <p>
     * Objects of immutable types do not have to be cloned, because they cannot be modified.
     * <p>
     * Note that this method does not do any sophisticated verification, it just checks if the specified class
     * corresponds to a small set of predefined conditions, e.g. primitive class, enum, String.
     *
     * @param cls the class to check
     * @return Whether the class represents immutable objects
     */
    static boolean isImmutable(Class<?> cls) {
        return cls.isPrimitive() || cls.isEnum() || IMMUTABLE_TYPES.contains(cls);
    }

    /**
     * Checks if the specified object is immutable.
     * <p>
     * {@code null} is considered immutable, otherwise, this method just calls {@link #isImmutable(Class)}.
     *
     * @param object The instance to check
     * @return immutability status
     */
    static boolean isImmutable(Object object) {
        return object == null || isImmutable(object.getClass());
    }

    @Override
    public void mergeChanges(ObjectChangeSet changeSet) {
        final Object original = changeSet.getChangedObject();
        try {
            for (ChangeRecord change : changeSet.getChanges()) {
                Field f = change.getAttribute().getJavaField();
                if (isImmutable(f.getType())) {
                    EntityPropertiesUtils.setFieldValue(f, original, change.getNewValue());
                    continue;
                }
                Object origVal = EntityPropertiesUtils.getFieldValue(f, original);
                Object newVal = change.getNewValue();
                if (newVal == null) {
                    EntityPropertiesUtils.setFieldValue(f, original, null);
                    continue;
                }
                getInstanceBuilder(newVal).mergeChanges(f, original, origVal, newVal);
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

    @Override
    public void reset() {
        visitedEntities.clear();
    }

    @Override
    public void removeVisited(Object instance, Descriptor descriptor) {
        visitedEntities.remove(descriptor, instance);
    }

    /**
     * Gets basic object info for logging.
     * <p>
     * This works around using {@link Object#toString()} for entities, which could inadvertently trigger lazy field
     * fetching.
     *
     * @param object Object to stringify
     * @return String info about the specified object
     */
    private String stringify(Object object) {
        assert object != null;
        return isTypeManaged(object.getClass()) ?
               (object.getClass().getSimpleName() + IdentifierTransformer.stringifyIri(
                       EntityPropertiesUtils.getIdentifier(object, getMetamodel()))) :
               object.toString();
    }

    private static Set<Class<?>> getImmutableTypes() {
        return Stream.of(Boolean.class,
                         Character.class,
                         Byte.class,
                         Short.class,
                         Integer.class,
                         Long.class,
                         Float.class,
                         Double.class,
                         BigInteger.class,
                         BigDecimal.class,
                         Void.class,
                         String.class,
                         URI.class,
                         URL.class,
                         LocalDate.class,
                         LocalTime.class,
                         LocalDateTime.class,
                         ZonedDateTime.class,
                         OffsetDateTime.class,
                         OffsetTime.class,
                         ZoneOffset.class,
                         Instant.class,
                         Duration.class,
                         Period.class,
                         LangString.class).collect(Collectors.toSet());
    }

    private final class Builders {
        private final AbstractInstanceBuilder defaultBuilder;
        private final AbstractInstanceBuilder dateBuilder;
        private final AbstractInstanceBuilder multilingualStringBuilder;
        // Lists and Sets
        private AbstractInstanceBuilder collectionBuilder;
        private AbstractInstanceBuilder mapBuilder;

        private Builders() {
            this.defaultBuilder = new DefaultInstanceBuilder(CloneBuilderImpl.this, uow);
            this.dateBuilder = new DateInstanceBuilder(CloneBuilderImpl.this, uow);
            this.multilingualStringBuilder = new MultilingualStringInstanceBuilder(CloneBuilderImpl.this, uow);
        }

        private AbstractInstanceBuilder getBuilder(Object toClone) {
            if (toClone instanceof Date) {
                return dateBuilder;
            }
            if (toClone instanceof MultilingualString) {
                return multilingualStringBuilder;
            }
            if (toClone instanceof Map) {
                if (mapBuilder == null) {
                    this.mapBuilder = new MapInstanceBuilder(CloneBuilderImpl.this, uow);
                }
                return mapBuilder;
            } else if (toClone instanceof Collection) {
                if (collectionBuilder == null) {
                    this.collectionBuilder = new CollectionInstanceBuilder(CloneBuilderImpl.this, uow);
                }
                return collectionBuilder;
            } else {
                return defaultBuilder;
            }
        }
    }
}
