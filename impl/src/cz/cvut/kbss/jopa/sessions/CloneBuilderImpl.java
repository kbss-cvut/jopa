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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.OWLPersistenceException;

public class CloneBuilderImpl implements CloneBuilder {

	private static Logger log = Logger.getLogger(CloneBuilderImpl.class
			.getName());

	private static final Set<Class<?>> WRAPPER_TYPES = getWrapperTypes();

	// This identity map stores visited objects during the clone building
	// process. We use this to prevent cloning already cloned objects.
	private Map<Object, Object> visitedObjects;
	private InstantiationHelper instantiationHelper;

	private UnitOfWorkImpl uow;

	public CloneBuilderImpl() {
		this.uow = null;
	}

	public CloneBuilderImpl(UnitOfWorkImpl uow) {
		this.uow = uow;
	}

	public Object buildClone(final Object original) {
		if (log.isLoggable(Level.CONFIG)) {
			log.config("Cloning object...");
		}
		if (original == null) {
			return null;
		}
		if (getVisitedObjects().containsKey(original)) {
			return getVisitedObjects().get(original);
		}
		if (uow.containsOriginal(original)) {
			return uow.getCloneForOriginal(original);
		}
		Object clone = getInstantiationHelper().buildNewInstance(
				original.getClass(), original);
		getVisitedObjects().put(original, clone);
		populateAttributes(original, clone);
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
	private void populateAttributes(final Object original, Object clone) {
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
				if (isPrimitiveOrString(f.getType())) {
					// The field is an immutable type
					f.set(clone, f.get(original));
				} else if (f.get(original) instanceof Collection) {
					Collection<?> c = (Collection<?>) getInstantiationHelper()
							.buildNewInstance(f.getType(), f.get(original));
					f.set(clone, c);
				} else if (f.getType().isArray()) {
					Object[] arr = cloneArray(f.get(original));
					f.set(clone, arr);
				} else {
					// Else we have a relationship and we need to clone its
					// target as well
					Object attValue = f.get(original);
					if (getVisitedObjects().containsKey(attValue)) {
						f.set(clone, getVisitedObjects().get(attValue));
						continue;
					}
					if (uow.containsOriginal(attValue)) {
						// If the reference is already managed
						f.set(clone, uow.getCloneForOriginal(attValue));
						continue;
					}
					Object toAssign = null;
					if (attValue != null
							&& uow.getManagedTypes().contains(
									attValue.getClass())) {
						toAssign = uow.registerExistingObject(attValue);
					} else {
						toAssign = buildClone(attValue);
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

	/**
	 * Creates a deep copy of the specified array.
	 * 
	 * @param array
	 *            The array to clone.
	 * @return Deep copy of the specified array.
	 */
	Object[] cloneArray(final Object array) {
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
				clonedArr[i] = buildClone(originalArr[i]);
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

	public List<?> buildClones(List<?> originals) {
		if (originals == null) {
			return null;
		}
		List<Object> result = new ArrayList<Object>();
		for (Object obj : originals) {
			result.add(buildClone(obj));
		}
		return result;
	}

	public ObjectChangeSet createObjectChangeSet(Object original, Object clone,
			UnitOfWorkChangeSet changeSet) {
		if (original == null) {
			return null;
		}
		ObjectChangeSet chs = new ObjectChangeSetImpl(original, clone, false,
				changeSet);
		return chs;
	}

	public Object mergeChanges(Object original, Object clone,
			ObjectChangeSet changeSet, MergeManager mergeManager) {
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
				if (origVal != null && this.uow.containsOriginal(origVal)) {
					this.mergeChangesOnManaged(origVal, change.getNewValue());
				} else if (origVal != null && containsManagedObjects(origVal)) {
					this.mergeChangesOnManaged(origVal, change.getNewValue());
				} else {
					// Otherwise we can simply assign the new value
					f.set(original, change.getNewValue());
				}
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

	/**
	 * Check if the specified entity contains other entities managed by the
	 * current persistence context. Currently, this method checks only
	 * attributes of the specified entity, it does not descend into the object
	 * hierarchy.
	 * 
	 * @param entity
	 *            The entity to examine.
	 * @return True if the entity contains other managed entities.
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 */
	protected boolean containsManagedObjects(Object entity)
			throws IllegalArgumentException, IllegalAccessException {
		Class<?> cls = entity.getClass();
		List<Field> fields = getAllFields(cls);
		for (Field f : fields) {
			if (!f.isAccessible()) {
				f.setAccessible(true);
			}
			Class<?> fieldClass = f.getType();
			Object val = f.get(entity);
			if (isPrimitiveOrString(fieldClass)) {
				continue;
			}
			if (val instanceof Collection) {
				Collection<?> c = (Collection<?>) val;
				Iterator<?> it = c.iterator();
				if (it.hasNext() && this.uow.containsOriginal(it.next())) {
					return true;
				}
			}
			if (this.uow.containsOriginal(val)) {
				return true;
			}
		}
		return false;
	}

	public void mergeChangesOnManaged(Object original, Object clone)
			throws IllegalArgumentException, IllegalAccessException {
		if (original == null || clone == null) {
			return;
		}
		Class<?> cls = original.getClass();
		List<Field> fields = getAllFields(cls);
		for (Field f : fields) {
			if (!f.isAccessible()) {
				f.setAccessible(true);
			}
			Object clVal = f.get(clone);
			Object origVal = f.get(original);
			if (!(clVal instanceof Collection)
					&& !uow.containsOriginal(origVal)) {
				f.set(original, clVal);
			} else {
				if (clVal instanceof Collection) {
					mergeCollections((Collection<?>) origVal,
							(Collection<?>) clVal);
				} else {
					mergeChangesOnManaged(origVal, clVal);
				}
			}
		}
	}

	private void mergeCollections(Collection orig, Collection clone)
			throws IllegalArgumentException, IllegalAccessException {
		if (clone.isEmpty()) {
			orig.clear();
			return;
		}
		Iterator<?> it = clone.iterator();
		Iterator<?> itOrig = orig.iterator();
		if (!orig.isEmpty()) {
			List<Object> clones = new ArrayList<Object>(clone);
			while (itOrig.hasNext()) {
				Object or = itOrig.next();
				if (uow.containsOriginal(or)) {
					Object cl = uow.getCloneForOriginal(or);
					if (clones.contains(cl)) {
						mergeChangesOnManaged(or, cl);
					} else {
						itOrig.remove();
					}
				} else {
					orig.clear();
					orig.addAll(clone);
					return;
				}
			}
		} else {
			while (it.hasNext()) {
				Object cl = it.next();
				if (uow.contains(cl)) {
					Object or = uow.getOriginal(cl);
					mergeChangesOnManaged(or, cl);
					orig.add(or);
				} else {
					orig.add(cl);
				}
			}
		}
	}

	private Map<Object, Object> getVisitedObjects() {
		if (this.visitedObjects == null) {
			this.visitedObjects = new IdentityHashMap<Object, Object>();
		}
		return this.visitedObjects;
	}

	protected InstantiationHelper getInstantiationHelper() {
		if (instantiationHelper == null) {
			this.instantiationHelper = new InstantiationHelper(this, uow);
		}
		return instantiationHelper;
	}

	public static synchronized boolean isFieldInferred(final Field f) {
		Annotation[] annots = f.getAnnotations();
		try {
			for (Annotation a : annots) {
				Method m = a.getClass().getDeclaredMethod("inferred",
						(Class<?>[]) null);
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

	public void reset() {
		getVisitedObjects().clear();
	}

	/**
	 * Get all fields declared for the specified class, except the fields
	 * declared as static. This method makes no assumptions and modifications of
	 * the field's accessibility.
	 * 
	 * @param cls
	 *            The class whose fields we want.
	 * @return List of fields declared by the class and all its super classes.
	 */
	public static synchronized List<Field> getAllFields(Class<?> cls) {
		List<Field> fields = new ArrayList<Field>();
		fields.addAll(Arrays.asList(cls.getDeclaredFields()));
		Class<?> tmp = cls.getSuperclass();
		while (tmp != null) {
			fields.addAll(Arrays.asList(tmp.getDeclaredFields()));
			tmp = tmp.getSuperclass();
		}
		Iterator<Field> it = fields.iterator();
		while (it.hasNext()) {
			Field f = it.next();
			if (Modifier.isStatic(f.getModifiers())) {
				it.remove();
			}
		}
		return fields;
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

}
