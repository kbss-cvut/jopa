package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

class MapInstanceBuilder extends AbstractInstanceBuilder {

    private static final Class<?> singletonMapClass = Collections.singletonMap(null, null)
                                                                 .getClass();

    MapInstanceBuilder(CloneBuilderImpl builder, UnitOfWork uow) {
        super(builder, uow);
        this.populates = true;
    }

    @Override
    Object buildClone(Object cloneOwner, Field field, Object original, Descriptor repository) {
        Map<?, ?> orig = (Map<?, ?>) original;
        if (original instanceof IndirectCollection) {
            orig = ((IndirectCollection<Map<?, ?>>) original).getReferencedCollection();
        }
        final Class<?> origCls = orig.getClass();
        Map<?, ?> clone;
        clone = cloneUsingDefaultConstructor(cloneOwner, field, origCls, orig, repository);
        if (clone == null) {
            if (singletonMapClass.isInstance(orig)) {
                clone = buildSingletonClone(cloneOwner, field, orig, repository);
            } else {
                throw new IllegalArgumentException("Unsupported map type " + origCls);
            }
        }
        clone = (Map<?, ?>) builder.createIndirectCollection(clone, cloneOwner, field);
        return clone;

    }

    private Map<?, ?> cloneUsingDefaultConstructor(Object cloneOwner, Field field,
                                                   Class<?> origCls, Map<?, ?> original, Descriptor repository) {
        Map<?, ?> result = createNewInstance(origCls, original.size());
        if (result != null) {
            cloneMapContent(cloneOwner, field, original, result, repository);
        }
        return result;
    }

    private Map<?, ?> createNewInstance(Class<?> type, int size) {
        Map<?, ?> result = null;
        final Class<?>[] types = {int.class};
        Object[] params;
        Constructor<?> c = getDeclaredConstructorFor(type, types);
        if (c != null) {
            params = new Object[1];
            params[0] = size;
        } else {
            c = getDeclaredConstructorFor(type, null);
            params = null;
        }
        if (c == null) {
            return null;
        }
        try {
            result = (Map<?, ?>) c.newInstance(params);
        } catch (InstantiationException | IllegalArgumentException | InvocationTargetException e) {
            throw new OWLPersistenceException(e);
        } catch (IllegalAccessException e) {
            logConstructorAccessException(c, e);
            try {
                result = (Map<?, ?>) AccessController
                        .doPrivileged(new PrivilegedInstanceCreator(c));
            } catch (PrivilegedActionException ex) {
                logPrivilegedConstructorAccessException(c, ex);
                // Do nothing
            }
        }
        return result;
    }

    private Map<?, ?> buildSingletonClone(Object cloneOwner, Field field, Map<?, ?> orig,
                                          Descriptor repository) {
        final Constructor<?> c = getFirstDeclaredConstructorFor(singletonMapClass);
        if (!c.isAccessible()) {
            c.setAccessible(true);
        }
        Entry<?, ?> e = orig.entrySet().iterator().next();
        Object key = CloneBuilderImpl.isPrimitiveOrString(e.getKey().getClass()) ? e.getKey()
                : cloneObject(cloneOwner, field, e.getKey(), repository);
        Object value = CloneBuilderImpl.isPrimitiveOrString(e.getValue().getClass()) ? e.getValue()
                : cloneObject(cloneOwner, field, e.getValue(), repository);
        if (value instanceof Collection || value instanceof Map) {
            value = builder.createIndirectCollection(value, cloneOwner, field);
        }
        try {
            return (Map<?, ?>) c.newInstance(key, value);
        } catch (IllegalAccessException ex) {
            logConstructorAccessException(c, ex);
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

    private void cloneMapContent(Object cloneOwner, Field field, Map<?, ?> source,
                                 Map<?, ?> target, Descriptor repository) {
        if (source.isEmpty()) {
            return;
        }
        Map<Object, Object> m = (Map<Object, Object>) target;
        Entry<?, ?> tmp = source.entrySet().iterator().next();
        boolean keyPrimitive = CloneBuilderImpl.isPrimitiveOrString(tmp.getKey().getClass());
        boolean valuePrimitive = CloneBuilderImpl.isPrimitiveOrString(tmp.getValue().getClass());
        for (Entry<?, ?> e : source.entrySet()) {
            Object key;
            Object value;
            if (keyPrimitive) {
                if (valuePrimitive) {
                    m.putAll(source);
                    break;
                }
                key = e.getKey();
                value = cloneObject(cloneOwner, field, e.getValue(), repository);
            } else {
                key = cloneObject(cloneOwner, field, e.getKey(), repository);
                value = valuePrimitive ? e.getValue() : cloneObject(cloneOwner, field,
                        e.getValue(), repository);
            }
            m.put(key, value);
        }
    }

    private Object cloneObject(Object owner, Field field, Object obj, Descriptor repository) {
        Object clone;
        if (obj == null) {
            clone = null;
        } else if (builder.isTypeManaged(obj.getClass())) {
            clone = uow.registerExistingObject(obj, repository);
        } else {
            clone = builder.buildClone(owner, field, obj, repository);
        }
        return clone;
    }

    @Override
    void mergeChanges(Field field, Object target, Object originalValue, Object cloneValue) {
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
            EntityPropertiesUtils.setFieldValue(field, target, orig);
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
