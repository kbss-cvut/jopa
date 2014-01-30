package cz.cvut.kbss.jopa.sessions;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.adapters.IndirectList;
import cz.cvut.kbss.jopa.adapters.IndirectMap;
import cz.cvut.kbss.jopa.adapters.IndirectSet;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

public class CloneBuilderImpl implements CloneBuilder {

	private static final Logger LOG = Logger.getLogger(CloneBuilderImpl.class.getName());

	private static final Set<Class<?>> WRAPPER_TYPES = getWrapperTypes();

	// This identity map stores visited objects during the clone building
	// process. We use this to prevent cloning already cloned objects.
	private final Map<Object, Object> visitedObjects;
	private final Map<URI, Map<Object, Object>> visitedEntities;

	private final Builders builders;

	private final UnitOfWorkImpl uow;

	public CloneBuilderImpl() {
		this.uow = null;
		this.visitedObjects = new IdentityHashMap<Object, Object>();
		this.visitedEntities = new HashMap<URI, Map<Object, Object>>();
		this.builders = new Builders();
	}

	public CloneBuilderImpl(UnitOfWorkImpl uow) {
		this.uow = uow;
		this.visitedObjects = new IdentityHashMap<Object, Object>();
		this.visitedEntities = new HashMap<URI, Map<Object, Object>>();
		this.builders = new Builders();
	}

	@Override
	public Object buildClone(final Object original, final URI contextUri) {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("Cloning object...");
		}
		if (original == null || contextUri == null) {
			throw new NullPointerException();
		}
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
			final Object visitedClone = getVisitedEntity(contextUri, pk);
			if (visitedClone != null) {
				return visitedClone;
			}
		}
		final AbstractInstanceBuilder builder = getInstanceBuilder(original);
		Object clone = builder.buildClone(null, original.getClass(), original, contextUri);
		visitedObjects.put(original, clone);
		if (!builder.populatesAttributes()) {
			populateAttributes(original, clone, contextUri);
		}
		if (managed) {
			final IRI pk = getIdentifier(clone);
			putVisitedEntity(contextUri, pk, clone);
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
	private void populateAttributes(final Object original, Object clone, final URI contextUri) {
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
					// Collection or a Map
					final Object clonedCollection = getInstanceBuilder(origVal).buildClone(clone,
							origClass, origVal, contextUri);
					Object toUse = createIndirectCollection(clonedCollection, clone);
					f.set(clone, toUse);
				} else if (f.getType().isArray()) {
					Object[] arr = cloneArray(origVal, contextUri);
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
						final IRI pk = getIdentifier(origVal);
						final Map<Object, Object> m = visitedEntities.get(contextUri);
						toAssign = (m != null && m.containsKey(pk)) ? m.get(pk) : uow
								.registerExistingObject(origVal, contextUri);
					} else {
						toAssign = buildClone(origVal, contextUri);
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
	 * @param URI
	 *            of the ontology context the original belongs to
	 * @return Deep copy of the specified array.
	 */
	Object[] cloneArray(final Object array, URI contextUri) {
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
						contextUri);
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
	public List<?> buildClones(Map<?, URI> originals) {
		if (originals == null) {
			return null;
		}
		List<Object> result = new ArrayList<Object>();
		for (Entry<?, URI> obj : originals.entrySet()) {
			result.add(buildClone(obj.getKey(), obj.getValue()));
		}
		return result;
	}

	@Override
	public ObjectChangeSet createObjectChangeSet(Object original, Object clone,
			UnitOfWorkChangeSet changeSet) {
		if (original == null) {
			return null;
		}
		ObjectChangeSet chs = new ObjectChangeSetImpl(original, clone, false, changeSet);
		return chs;
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

	private Object getVisitedEntity(URI ctx, Object primaryKey) {
		assert ctx != null;
		assert primaryKey != null;
		if (!visitedEntities.containsKey(ctx)) {
			return null;
		}
		return visitedEntities.get(ctx).get(primaryKey);
	}

	private void putVisitedEntity(URI ctx, Object primaryKey, Object entity) {
		assert ctx != null;
		assert primaryKey != null;
		assert entity != null;
		Map<Object, Object> ctxMap = visitedEntities.get(ctx);
		if (ctxMap == null) {
			ctxMap = new HashMap<Object, Object>();
			visitedEntities.put(ctx, ctxMap);
		}
		ctxMap.put(primaryKey, entity);
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

	IndirectCollection<?> createIndirectCollection(Object c, Object owner) {
		IndirectCollection<?> res = null;
		if (c instanceof List) {
			res = new IndirectList<>(owner, uow, (List<?>) c);
		} else if (c instanceof Set) {
			res = new IndirectSet<>(owner, uow, (Set<?>) c);
		} else if (c instanceof Map) {
			res = new IndirectMap<>(owner, uow, (Map<?, ?>) c);
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
