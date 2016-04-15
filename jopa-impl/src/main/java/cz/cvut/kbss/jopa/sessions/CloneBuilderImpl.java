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

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.net.URI;
import java.net.URL;
import java.util.*;

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
    public Object buildClone(Object original, Descriptor descriptor) {
        if (original == null || descriptor == null) {
            throw new NullPointerException();
        }
        LOG.trace("Cloning object {}.", original);
        return buildCloneImpl(null, null, original, descriptor);
    }

    @Override
    public Object buildClone(Object cloneOwner, Field clonedField, Object original,
                             Descriptor descriptor) {
        if (cloneOwner == null || original == null || descriptor == null) {
            throw new NullPointerException();
        }
        LOG.trace("Cloning object {} with owner {}", original, cloneOwner);
        return buildCloneImpl(cloneOwner, clonedField, original, descriptor);
    }

    private Object buildCloneImpl(Object cloneOwner, Field clonedField, Object original,
                                  Descriptor descriptor) {
        if (isOriginalInUoW(original)) {
            return uow.getCloneForOriginal(original);
        }
        final Class<?> cls = original.getClass();
        final boolean managed = isTypeManaged(cls);
        if (managed) {
            final Object visitedClone = getVisitedEntity(descriptor, original);
            if (visitedClone != null) {
                return visitedClone;
            }
        }
        final AbstractInstanceBuilder builder = getInstanceBuilder(original);
        Object clone = builder.buildClone(cloneOwner, clonedField, original, descriptor);
        if (managed) {
            // Register visited object before populating attributes to prevent endless cloning cycles
            putVisitedEntity(descriptor, original, clone);
        }
        if (!builder.populatesAttributes() && !isImmutable(original.getClass())) {
            populateAttributes(original, clone, descriptor);
        }
        return clone;
    }

    /**
     * Clone all the attributes of the original and set the clone values. This also means cloning any relationships and
     * their targets.
     *
     * @param original Original
     * @param clone    Object
     */
    private void populateAttributes(final Object original, Object clone, final Descriptor descriptor) {
        final Class<?> originalClass = original.getClass();
        final EntityType<?> et = getMetamodel().entity(originalClass);
        for (FieldSpecification<?, ?> fs : et.getFieldSpecifications()) {
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
            } else if (origVal instanceof Collection || origVal instanceof Map) {
                final Descriptor fieldDescriptor = getFieldDescriptor(f, originalClass, descriptor);
                // Collection or Map
                clonedValue = getInstanceBuilder(origVal).buildClone(clone, f, origVal, fieldDescriptor);
            } else {
                // Otherwise we have a relationship and we need to clone its target as well
                if (isOriginalInUoW(origVal)) {
                    // If the reference is already managed
                    clonedValue = uow.getCloneForOriginal(origVal);
                } else {
                    if (isTypeManaged(origValueClass)) {
                        final Descriptor fieldDescriptor = getFieldDescriptor(f, originalClass, descriptor);
                        clonedValue = getVisitedEntity(descriptor, origVal);
                        if (clonedValue == null) {
                            clonedValue = uow.registerExistingObject(origVal, fieldDescriptor);
                        }
                    } else {
                        clonedValue = buildClone(origVal, descriptor);
                    }
                }
            }
            EntityPropertiesUtils.setFieldValue(f, clone, clonedValue);
        }
        cloneIdentifier(original, clone, et);
    }

    private Descriptor getFieldDescriptor(Field field, Class<?> entityClass, Descriptor entityDescriptor) {
        final EntityType<?> et = getMetamodel().entity(entityClass);
        final FieldSpecification<?, ?> fieldSpec = et.getFieldSpecification(field.getName());
        return entityDescriptor.getAttributeDescriptor(fieldSpec);
    }

    private void cloneIdentifier(Object original, Object clone, EntityType<?> et) {
        final Identifier identifier = et.getIdentifier();
        final Object idValue = EntityPropertiesUtils.getFieldValue(identifier.getJavaField(), original);
        EntityPropertiesUtils.setFieldValue(identifier.getJavaField(), clone, idValue);
    }

    /**
     * Check if the given class is an immutable type. This is used by the {@link
     * #populateAttributes(Object, Object, Descriptor)} method. If this returns true, the populateAttributes can simply
     * assign the value.
     *
     * @param cls the class to check
     * @return Whether the class represents immutable objects
     */
    public static boolean isImmutable(final Class<?> cls) {
        return cls.isPrimitive() || cls.isEnum()|| IMMUTABLE_TYPES.contains(cls);
    }

    @Override
    public void mergeChanges(Object original, ObjectChangeSet changeSet) {
        Map<String, ChangeRecord> changes = changeSet.getChanges();
        try {
            for (String att : changes.keySet()) {
                ChangeRecord change = changes.get(att);
                Field f = original.getClass().getDeclaredField(att);
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
        } catch (NoSuchFieldException | SecurityException e) {
            throw new OWLPersistenceException(e);
        }
    }

    Object getVisitedEntity(Descriptor descriptor, Object original) {
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
        return uow.isTypeManaged(cls);
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

    IndirectCollection<?> createIndirectCollection(Object c, Object owner, Field f) {
        return uow.createIndirectCollection(c, owner, f);
    }

    public static synchronized boolean isFieldInferred(final Field f) {
        return f.getAnnotation(Inferred.class) != null;
    }

    private static Set<Class<?>> getImmutableTypes() {
        HashSet<Class<?>> ret = new HashSet<>();
        ret.add(Boolean.class);
        ret.add(Character.class);
        ret.add(Byte.class);
        ret.add(Short.class);
        ret.add(Integer.class);
        ret.add(Long.class);
        ret.add(Float.class);
        ret.add(Double.class);
        ret.add(Void.class);
        ret.add(String.class);
        ret.add(URI.class);
        ret.add(URL.class);
        return ret;
    }

    private final class Builders {
        private AbstractInstanceBuilder defaultBuilder;
        private AbstractInstanceBuilder dateBuilder;
        // Lists and Sets
        private AbstractInstanceBuilder collectionBuilder;
        private AbstractInstanceBuilder mapBuilder;

        private Builders() {
            this.defaultBuilder = new DefaultInstanceBuilder(CloneBuilderImpl.this, uow);
            this.dateBuilder = new DateInstanceBuilder(CloneBuilderImpl.this, uow);
        }

        private AbstractInstanceBuilder getBuilder(Object toClone) {
            if (toClone instanceof Date) {
                return dateBuilder;
            }
            if (toClone instanceof Map) {
                if (mapBuilder == null) {
                    this.mapBuilder = new MapInstanceBuilder(CloneBuilderImpl.this, uow);
                }
                return mapBuilder;
            } else if (toClone instanceof Collection) {
                if (collectionBuilder == null) {
                    this.collectionBuilder = new CollectionInstanceBuilder(CloneBuilderImpl.this,
                            uow);
                }
                return collectionBuilder;
            } else {
                return defaultBuilder;
            }
        }
    }
}
