package cz.cvut.kbss.jopa.sessions;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.adapters.IndirectList;
import cz.cvut.kbss.jopa.adapters.IndirectMap;
import cz.cvut.kbss.jopa.adapters.IndirectSet;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

public class CloneBuilderImpl implements CloneBuilder {

	private static final Logger LOG = Logger.getLogger(CloneBuilderImpl.class.getName());

	private static final Set<Class<?>> WRAPPER_TYPES = getWrapperTypes();

	// The identity map stores objects visited during the clone building
	// process. We use this to prevent cloning already cloned objects.
	private final Map<Object, Object> visitedObjects;
	private final RepositoryMap visitedEntities;

	private final Builders builders;

	private final UnitOfWorkImpl uow;

	public CloneBuilderImpl(UnitOfWorkImpl uow) {
		this.uow = uow;
		this.visitedObjects = new IdentityHashMap<Object, Object>();
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

		if (visitedObjects.containsKey(original)) {
			return visitedObjects.get(original);
		}
		if (isOriginalInUoW(original)) {
			return uow.getCloneForOriginal(original);
		}
		final Class<?> cls = original.getClass();
		final boolean managed = isTypeManaged(cls);
		if (managed) {
			final IRI pk = getIdentifier(original);
			final Object visitedClone = getVisitedEntity(descriptor, pk);
			if (visitedClone != null) {
				return visitedClone;
			}
		}
		final AbstractInstanceBuilder builder = getInstanceBuilder(original);
		Object clone = builder.buildClone(cloneOwner, clonedField, original, descriptor);
		visitedObjects.put(original, clone);
		if (!builder.populatesAttributes() && !isPrimitiveOrString(original.getClass())) {
			populateAttributes(original, clone, descriptor);
		}
		if (managed) {
			final IRI pk = getIdentifier(clone);
			putVisitedEntity(descriptor, pk, clone);
		}
		return clone;
	}

	/**
	 * Clone all the attributes of the original and set the clone values. This
	 * also means cloning any relationships and their targets.
	 * 
	 * @param original
	 *            Original
	 * @param clone
	 *            Object
	 */
	private void populateAttributes(final Object original, Object clone, final Descriptor descriptor) {
		Class<?> theClass = original.getClass();
		List<Field> fields = new ArrayList<Field>();
		fields.addAll(Arrays.asList(theClass.getDeclaredFields()));
		Class<?> tmp = theClass.getSuperclass();
		while (tmp != null) {
			fields.addAll(Arrays.asList(tmp.getDeclaredFields()));
			tmp = tmp.getSuperclass();
		}
		try {
			for (Field f : fields) {
				if (Modifier.isStatic(f.getModifiers())) {
					continue;
				}
				if (!f.isAccessible()) {
					f.setAccessible(true);
				}
				final Object origVal = f.get(original);
				if (origVal == null) {
					continue;
				}
				final Class<?> origClass = origVal.getClass();
				if (isPrimitiveOrString(origClass)) {
					// The field is an immutable type
					f.set(clone, origVal);
				} else if (origVal instanceof Collection || origVal instanceof Map) {
					final Descriptor fieldDescriptor = getFieldDescriptor(f, theClass, descriptor);
					// Collection or a Map
					final Object clonedCollection = getInstanceBuilder(origVal).buildClone(clone,
							f, origVal, fieldDescriptor);
					f.set(clone, clonedCollection);
				} else if (f.getType().isArray()) {
					final Descriptor fieldDescriptor = getFieldDescriptor(f, theClass, descriptor);
					Object[] arr = cloneArray(origVal, fieldDescriptor);
					f.set(clone, arr);
				} else {
					// Else we have a relationship and we need to clone its
					// target as well
					if (visitedObjects.containsKey(origVal)) {
						f.set(clone, visitedObjects.get(origVal));
						continue;
					}
					if (isOriginalInUoW(origVal)) {
						// If the reference is already managed
						f.set(clone, uow.getCloneForOriginal(origVal));
						continue;
					}
					Object toAssign = null;
					if (isTypeManaged(origClass)) {
						final Descriptor fieldDescriptor = getFieldDescriptor(f, theClass,
								descriptor);
						final IRI pk = getIdentifier(origVal);
						toAssign = getVisitedEntity(descriptor, pk);
						if (toAssign == null) {
							toAssign = uow.registerExistingObject(origVal, fieldDescriptor);
						}
					} else {
						toAssign = buildClone(origVal, descriptor);
					}
					f.set(clone, toAssign);
				}
			}
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			throw new OWLPersistenceException("Error while cloning object.", e);
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
			throw new OWLPersistenceException("Error while cloning object.", e);
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
	 * @param array
	 *            The array to clone.
	 * @param URI
	 *            of the ontology context the original belongs to
	 * @return Deep copy of the specified array.
	 */
	Object[] cloneArray(final Object array, Descriptor repository) {
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
						repository);
			}
		}
		return clonedArr;
	}

	/**
	 * Check if the given class is of primitive, String or Enum type. This is
	 * used by the {@link #populateAttributes(Object, Object)} method. If this
	 * returns true, the populateAttributes can simply assign the value.
	 * 
	 * @param cls
	 *            Class<?>
	 * @return boolean
	 */
	public static boolean isPrimitiveOrString(final Class<?> cls) {
		return cls.isPrimitive() || String.class.equals(cls) || cls.isEnum()
				|| WRAPPER_TYPES.contains(cls);
	}

	@Override
	public Object mergeChanges(Object original, Object clone, ObjectChangeSet changeSet,
			MergeManager mergeManager) {
		Map<String, ChangeRecord> changes = changeSet.getAttributesToChange();
		try {
			for (String att : changes.keySet()) {
				ChangeRecord change = changes.get(att);
				Field f = original.getClass().getDeclaredField(att);
				if (!f.isAccessible()) {
					f.setAccessible(true);
				}
				if (isPrimitiveOrString(f.getClass())) {
					f.set(original, change.getNewValue());
					continue;
				}
				Object origVal = f.get(original);
				Object newVal = change.getNewValue();
				if (newVal == null) {
					f.set(original, null);
					continue;
				}
				getInstanceBuilder(newVal).mergeChanges(f, original, origVal, newVal);
			}
		} catch (NoSuchFieldException e) {
			throw new OWLPersistenceException(e);
		} catch (SecurityException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		}
		return original;
	}

	private Object getVisitedEntity(Descriptor descriptor, Object primaryKey) {
		assert descriptor != null;
		assert primaryKey != null;
		return visitedEntities.get(descriptor, primaryKey);
	}

	private void putVisitedEntity(Descriptor descriptor, Object primaryKey, Object entity) {
		assert descriptor != null;
		visitedEntities.add(descriptor, primaryKey, entity);
	}

	private IRI getIdentifier(Object entity) {
		assert entity != null;
		assert uow.isManagedType(entity.getClass());
		return EntityPropertiesUtils.getPrimaryKey(entity, uow.getMetamodel());
	}

	AbstractInstanceBuilder getInstanceBuilder(Object toClone) {
		return builders.getBuilder(toClone);
	}

	boolean isTypeManaged(Class<?> cls) {
		return uow.isManagedType(cls);
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
		visitedObjects.clear();
		visitedEntities.clear();
	}

	IndirectCollection<?> createIndirectCollection(Object c, Object owner, Field f) {
		IndirectCollection<?> res = null;
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
		Annotation[] annots = f.getAnnotations();
		try {
			for (Annotation a : annots) {
				Method m = a.getClass().getDeclaredMethod("inferred", (Class<?>[]) null);
				return (Boolean) m.invoke(a, (Object[]) null);
			}
		} catch (NoSuchMethodException e) {
			return false;
		} catch (IllegalAccessException e) {
			return false;
		} catch (IllegalArgumentException e) {
			return false;
		} catch (InvocationTargetException e) {
			return false;
		}
		return false;
	}

	private static Set<Class<?>> getWrapperTypes() {
		HashSet<Class<?>> ret = new HashSet<Class<?>>();
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
		// Lists and Sets
		private AbstractInstanceBuilder collectionBuilder;
		private AbstractInstanceBuilder mapBuilder;

		private Builders() {
			this.defaultBuilder = new DefaultInstanceBuilder(CloneBuilderImpl.this, uow);
		}

		private AbstractInstanceBuilder getBuilder(Object toClone) {
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
