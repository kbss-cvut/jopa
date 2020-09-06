/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
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

    MapInstanceBuilder(CloneBuilderImpl builder, UnitOfWorkImpl uow) {
        super(builder, uow);
    }

    @Override
    Object buildClone(Object cloneOwner, Field field, Object original, CloneConfiguration configuration) {
        Map<?, ?> orig = (Map<?, ?>) original;
        if (original instanceof IndirectCollection) {
            orig = ((IndirectCollection<Map<?, ?>>) original).unwrap();
        }
        final Class<?> origCls = orig.getClass();
        Map<?, ?> clone;
        clone = cloneUsingDefaultConstructor(cloneOwner, field, origCls, orig, configuration);
        if (clone == null) {
            if (singletonMapClass.isInstance(orig)) {
                clone = buildSingletonClone(cloneOwner, field, orig, configuration);
            } else {
                throw new IllegalArgumentException("Unsupported map type " + origCls);
            }
        }
        clone = (Map<?, ?>) uow.createIndirectCollection(clone, cloneOwner, field);
        return clone;

    }

    private Map<?, ?> cloneUsingDefaultConstructor(Object cloneOwner, Field field, Class<?> origCls, Map<?, ?> original,
                                                   CloneConfiguration configuration) {
        Map<?, ?> result = createNewInstance(origCls, original.size());
        if (result != null) {
            cloneMapContent(cloneOwner, field, original, result, configuration);
        }
        return result;
    }

    private static Map<?, ?> createNewInstance(Class<?> type, int size) {
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
                                          CloneConfiguration configuration) {
        Entry<?, ?> e = orig.entrySet().iterator().next();
        Object key = CloneBuilderImpl.isImmutable(e.getKey()) ? e.getKey() :
                     cloneObject(cloneOwner, field, e.getKey(), configuration);
        Object value = CloneBuilderImpl.isImmutable(e.getValue()) ? e.getValue() :
                       cloneObject(cloneOwner, field, e.getValue(), configuration);
        if (value instanceof Collection || value instanceof Map) {
            value = uow.createIndirectCollection(value, cloneOwner, field);
        }
        return Collections.singletonMap(key, value);
    }

    private void cloneMapContent(Object cloneOwner, Field field, Map<?, ?> source,
                                 Map<?, ?> target, CloneConfiguration configuration) {
        if (source.isEmpty()) {
            return;
        }
        final Map<Object, Object> m = (Map<Object, Object>) target;
        final Entry<?, ?> tmp = source.entrySet().iterator().next();
        // Note: If we encounter null -> null mapping first, the whole map will be treated as immutable type map, which can be incorrect
        final boolean keyPrimitive = CloneBuilderImpl.isImmutable(tmp.getKey());
        final boolean valuePrimitive = CloneBuilderImpl.isImmutable(tmp.getValue());
        for (Entry<?, ?> e : source.entrySet()) {
            Object key;
            Object value;
            if (keyPrimitive) {
                if (valuePrimitive) {
                    m.putAll(source);
                    break;
                }
                key = e.getKey();
                value = cloneObject(cloneOwner, field, e.getValue(), configuration);
            } else {
                key = cloneObject(cloneOwner, field, e.getKey(), configuration);
                value = valuePrimitive ? e.getValue() : cloneObject(cloneOwner, field, e.getValue(), configuration);
            }
            m.put(key, value);
        }
    }

    private Object cloneObject(Object owner, Field field, Object obj, CloneConfiguration configuration) {
        Object clone;
        if (obj == null) {
            clone = null;
        } else if (builder.isTypeManaged(obj.getClass())) {
            clone = uow.registerExistingObject(obj, configuration.getDescriptor(), configuration.getPostRegister());
        } else {
            clone = builder.buildClone(owner, field, obj, configuration.getDescriptor());
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
            clone = ((IndirectCollection<Map<Object, Object>>) clone).unwrap();
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

    private static Map<Object, Object> createDefaultMap(int size) {
        return new HashMap<>(size > 1 ? size : 16);
    }

    @Override
    boolean populatesAttributes() {
        return true;
    }
}
