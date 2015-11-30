package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.adapters.IndirectList;
import cz.cvut.kbss.jopa.adapters.IndirectMap;
import cz.cvut.kbss.jopa.adapters.IndirectSet;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

public class CloneBuilderImpl implements CloneBuilder {

    private static final Logger LOG = Logger.getLogger(CloneBuilderImpl.class.getName());

    private static final Set<Class<?>> WRAPPER_TYPES = getWrapperTypes();

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
        if (LOG.isLoggable(Level.FINER)) {
            LOG.finer("Cloning object " + original);
        }
        return buildCloneImpl(null, null, original, descriptor);
    }

    @Override
    public Object buildClone(Object cloneOwner, Field clonedField, Object original,
                             Descriptor descriptor) {
        if (cloneOwner == null || original == null || descriptor == null) {
            throw new NullPointerException();
        }
        if (LOG.isLoggable(Level.FINER)) {
            LOG.finer("Cloning object " + original + " with owner " + cloneOwner);
        }
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
        if (!builder.populatesAttributes() && !isPrimitiveOrString(original.getClass())) {
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
        // TODO This should be refactored
        Class<?> theClass = original.getClass();
        List<Field> fields = new ArrayList<>();
        fields.addAll(Arrays.asList(theClass.getDeclaredFields()));
        Class<?> tmp = theClass.getSuperclass();
        while (tmp != null) {
            fields.addAll(Arrays.asList(tmp.getDeclaredFields()));
            tmp = tmp.getSuperclass();
        }
        for (Field f : fields) {
            if (EntityPropertiesUtils.isFieldTransient(f)) {
                continue;
            }
            final Object origVal = EntityPropertiesUtils.getFieldValue(f, original);
            if (origVal == null) {
                continue;
            }
            final Class<?> origClass = origVal.getClass();
            if (isPrimitiveOrString(origClass)) {
                // The field is an immutable type
                EntityPropertiesUtils.setFieldValue(f, clone, origVal);
            } else if (origVal instanceof Collection || origVal instanceof Map) {
                final Descriptor fieldDescriptor = getFieldDescriptor(f, theClass, descriptor);
                // Collection or a Map
                final Object clonedCollection = getInstanceBuilder(origVal).buildClone(clone,
                        f, origVal, fieldDescriptor);
                EntityPropertiesUtils.setFieldValue(f, clone, clonedCollection);
            } else if (f.getType().isArray()) {
                final Descriptor fieldDescriptor = getFieldDescriptor(f, theClass, descriptor);
                Object[] arr = cloneArray(origVal, fieldDescriptor);
                EntityPropertiesUtils.setFieldValue(f, clone, arr);
            } else {
                // Else we have a relationship and we need to clone its target as well
                if (isOriginalInUoW(origVal)) {
                    // If the reference is already managed
                    EntityPropertiesUtils.setFieldValue(f, clone, uow.getCloneForOriginal(origVal));
                    continue;
                }
                Object toAssign;
                if (isTypeManaged(origClass)) {
                    final Descriptor fieldDescriptor = getFieldDescriptor(f, theClass,
                            descriptor);
                    toAssign = getVisitedEntity(descriptor, origVal);
                    if (toAssign == null) {
                        toAssign = uow.registerExistingObject(origVal, fieldDescriptor);
                    }
                } else {
                    toAssign = buildClone(origVal, descriptor);
                }
                EntityPropertiesUtils.setFieldValue(f, clone, toAssign);
            }
        }
    }

    private Descriptor getFieldDescriptor(Field field, Class<?> entityClass,
                                          Descriptor entityDescriptor) {
        final EntityType<?> et = getMetamodel().entity(entityClass);
        final FieldSpecification<?, ?> fieldSpec = et.getFieldSpecification(field.getName());
        return entityDescriptor.getAttributeDescriptor(fieldSpec);
    }

    /**
     * Creates a deep copy of the specified array.
     *
     * @param array      The array to clone.
     * @param descriptor Context descriptor
     * @return Deep copy of the specified array.
     */
    Object[] cloneArray(final Object array, Descriptor descriptor) {
        if (array == null) {
            return null;
        }
        Object[] originalArr = (Object[]) array;
        Object[] clonedArr = Arrays.copyOf(originalArr, originalArr.length);
        if (clonedArr.length == 0) {
            return clonedArr;
        }
        Class<?> c = null;
        int j = 0;
        while (j < clonedArr.length) {
            if (clonedArr[j] != null) {
                c = clonedArr[j].getClass();
                break;
            }
            j++;
        }
        if (c == null) {
            return clonedArr;
        }
        if (isPrimitiveOrString(c)) {
            return clonedArr;
        } else {
            for (int i = 0; i < clonedArr.length; i++) {
                clonedArr[i] = originalArr[i] == null ? null : buildClone(originalArr[i],
                        descriptor);
            }
        }
        return clonedArr;
    }

    /**
     * Check if the given class is of primitive, String or Enum type. This is used by the {@link
     * #populateAttributes(Object, Object, Descriptor)} method. If this returns true, the populateAttributes can simply
     * assign the value.
     *
     * @param cls Class<?>
     * @return boolean
     */
    public static boolean isPrimitiveOrString(final Class<?> cls) {
        return cls.isPrimitive() || String.class.equals(cls) || cls.isEnum()
                || WRAPPER_TYPES.contains(cls) || URI.class.equals(cls);
        // TODO Check that URI is effectively final. Also consider adding URL here
    }

    @Override
    public void mergeChanges(Object original, ObjectChangeSet changeSet) {
        Map<String, ChangeRecord> changes = changeSet.getChanges();
        try {
            for (String att : changes.keySet()) {
                ChangeRecord change = changes.get(att);
                Field f = original.getClass().getDeclaredField(att);
                if (isPrimitiveOrString(f.getType())) {
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
        final IndirectCollection<?> res;
        if (c instanceof List) {
            res = new IndirectList<>(owner, f, uow, (List<?>) c);
        } else if (c instanceof Set) {
            res = new IndirectSet<>(owner, f, uow, (Set<?>) c);
        } else if (c instanceof Map) {
            res = new IndirectMap<>(owner, f, uow, (Map<?, ?>) c);
        } else {
            throw new UnsupportedOperationException("Unsupported collection type " + c.getClass());
        }
        return res;
    }

    public static synchronized boolean isFieldInferred(final Field f) {
        return f.getAnnotation(Inferred.class) != null;
    }

    private static Set<Class<?>> getWrapperTypes() {
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
