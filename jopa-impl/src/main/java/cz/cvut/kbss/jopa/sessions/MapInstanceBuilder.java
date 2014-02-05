package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

class MapInstanceBuilder extends AbstractInstanceBuilder {

	private static final Class<?> singletonMapClass = Collections.singletonMap(null, null)
			.getClass();

	MapInstanceBuilder(CloneBuilderImpl builder, UnitOfWork uow) {
		super(builder, uow);
		this.populates = true;
	}

	@Override
	Object buildClone(Object cloneOwner, Class<?> origCls, Object original, URI contextUri) {
		Map<?, ?> orig = (Map<?, ?>) original;
		if (original instanceof IndirectCollection) {
			orig = ((IndirectCollection<Map<?, ?>>) original).getReferencedCollection();
			origCls = orig.getClass();
		}
		Map<?, ?> clone = null;
		clone = cloneUsingDefaultConstructor(cloneOwner, origCls, orig, contextUri);
		if (clone == null) {
			if (singletonMapClass.isInstance(orig)) {
				clone = buildSingletonClone(cloneOwner, orig, contextUri);
			} else {
				throw new IllegalArgumentException("Unsupported map type " + origCls);
			}
		}
		return clone;

	}

	private Map<?, ?> cloneUsingDefaultConstructor(Object cloneOwner, Class<?> origCls,
			Map<?, ?> original, URI contextUri) {
		Map<?, ?> result = createNewInstance(origCls, original.size());
		if (result != null) {
			cloneMapContent(cloneOwner, original, result, contextUri);
		}
		return result;
	}

	private Map<?, ?> createNewInstance(Class<?> type, int size) {
		Map<?, ?> result = null;
		final Class<?>[] types = { int.class };
		Object[] params = null;
		Constructor<?> c = getDeclaredConstructorFor(type, types);
		if (c != null) {
			params = new Object[1];
			params[0] = Integer.valueOf(size);
		} else {
			c = getDeclaredConstructorFor(type, null);
			params = null;
		}
		if (c == null) {
			return result;
		}
		try {
			result = (Map<?, ?>) c.newInstance(params);
		} catch (InstantiationException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalAccessException e) {
			try {
				result = (Map<?, ?>) AccessController
						.doPrivileged(new PrivilegedInstanceCreator(c));
			} catch (PrivilegedActionException ex) {
				// Do nothing
			}
		} catch (IllegalArgumentException | InvocationTargetException e) {
			throw new OWLPersistenceException(e);
		}
		return result;
	}

	private Map<?, ?> buildSingletonClone(Object cloneOwner, Map<?, ?> orig, URI contextUri) {
		final Constructor<?> c = getFirstDeclaredConstructorFor(singletonMapClass);
		if (!c.isAccessible()) {
			c.setAccessible(true);
		}
		Entry<?, ?> e = orig.entrySet().iterator().next();
		Object key = CloneBuilderImpl.isPrimitiveOrString(e.getKey().getClass()) ? e.getKey()
				: cloneObject(e.getKey(), contextUri);
		Object value = CloneBuilderImpl.isPrimitiveOrString(e.getValue().getClass()) ? e.getValue()
				: cloneObject(e.getValue(), contextUri);
		if (value instanceof Collection || value instanceof Map) {
			value = builder.createIndirectCollection(value, cloneOwner);
		}
		try {
			return (Map<?, ?>) c.newInstance(key, value);
		} catch (IllegalAccessException ex) {
			try {
				return (Map<?, ?>) AccessController.doPrivileged(new PrivilegedInstanceCreator(c,
						key, value));
			} catch (PrivilegedActionException exx) {
				throw new OWLPersistenceException(exx);
			}
		} catch (InstantiationException | IllegalArgumentException | InvocationTargetException ex) {
			throw new OWLPersistenceException(ex);
		}
	}

	private void cloneMapContent(Object cloneOwner, Map<?, ?> source, Map<?, ?> target,
			URI contextUri) {
		if (source.isEmpty()) {
			return;
		}
		Map<Object, Object> m = (Map<Object, Object>) target;
		Entry<?, ?> tmp = source.entrySet().iterator().next();
		boolean keyPrimitive = CloneBuilderImpl.isPrimitiveOrString(tmp.getKey().getClass());
		boolean valuePrimitive = CloneBuilderImpl.isPrimitiveOrString(tmp.getValue().getClass());
		for (Entry<?, ?> e : source.entrySet()) {
			Object key = null;
			Object value = null;
			if (keyPrimitive) {
				if (valuePrimitive) {
					m.putAll(source);
					break;
				}
				key = e.getKey();
				value = cloneObject(e.getValue(), contextUri);
			} else {
				key = cloneObject(e.getKey(), contextUri);
				value = valuePrimitive ? e.getValue() : cloneObject(e.getValue(), contextUri);
			}
			if (value instanceof Collection || value instanceof Map) {
				value = builder.createIndirectCollection(value, cloneOwner);
			}
			m.put(key, value);
		}
	}

	private Object cloneObject(Object obj, URI contextUri) {
		Object clone;
		if (obj == null) {
			clone = null;
		} else if (builder.isTypeManaged(obj.getClass())) {
			clone = uow.registerExistingObject(obj, contextUri);
		} else {
			clone = builder.buildClone(obj, contextUri);
		}
		return clone;
	}

	@Override
	void mergeChanges(Field field, Object target, Object originalValue, Object cloneValue)
			throws IllegalArgumentException, IllegalAccessException {
		assert (originalValue == null) || (originalValue instanceof Map);
		assert cloneValue instanceof Map;

		Map<Object, Object> orig = (Map<Object, Object>) originalValue;
		Map<Object, Object> clone = (Map<Object, Object>) cloneValue;
		if (clone instanceof IndirectCollection) {
			clone = ((IndirectCollection<Map<Object, Object>>) clone).getReferencedCollection();
		}
		if (orig == null) {
			orig = (Map<Object, Object>) createNewInstance(clone.getClass(), clone.size());
			if (orig == null) {
				orig = createDefaultMap(clone.size());
			}
			field.set(target, orig);
		}
		orig.clear();
		if (clone.isEmpty()) {
			return;
		}
		for (Entry<?, ?> e : clone.entrySet()) {
			final Object key = e.getKey();
			final Object value = e.getValue();
			final Object keyToPut = uow.contains(key) ? builder.getOriginal(key) : key;
			final Object valueToPut = uow.contains(value) ? builder.getOriginal(value) : value;
			orig.put(keyToPut, valueToPut);
		}
	}

	private Map<Object, Object> createDefaultMap(int size) {
		return new HashMap<>(size > 1 ? size : 16);
	}
}
