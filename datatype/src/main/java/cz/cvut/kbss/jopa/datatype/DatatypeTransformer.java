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
package cz.cvut.kbss.jopa.datatype;

import cz.cvut.kbss.jopa.datatype.exception.DatatypeMappingException;
import cz.cvut.kbss.jopa.datatype.exception.UnsupportedTypeTransformationException;
import cz.cvut.kbss.jopa.datatype.util.Pair;
import cz.cvut.kbss.ontodriver.model.LangString;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.math.BigInteger;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

/**
 * Utility containing transformation rules for selected basic types.
 */
public class DatatypeTransformer {

    private static final Map<Pair, Function<Object, ?>> TRANSFORMERS = initTransformers();

    private static final Map<Class<?>, Class<?>> WRAPPER_TO_PRIMITIVES = Map.of(
            Integer.class, int.class,
            Boolean.class, boolean.class,
            Byte.class, byte.class,
            Short.class, short.class,
            Long.class, long.class,
            Float.class, float.class,
            Double.class, double.class
    );

    private DatatypeTransformer() {
        throw new AssertionError();
    }

    private static Map<Pair, Function<Object, ?>> initTransformers() {
        final Map<Pair, Function<Object, ?>> map = new HashMap<>();
        map.put(new Pair(Short.class, Integer.class), value -> ((Short) value).intValue());
        map.put(new Pair(Short.class, Long.class), value -> ((Short) value).longValue());
        map.put(new Pair(Short.class, Float.class), value -> ((Short) value).floatValue());
        map.put(new Pair(Short.class, Double.class), value -> ((Short) value).doubleValue());
        map.put(new Pair(Integer.class, Short.class), value -> ((Integer) value).shortValue());
        map.put(new Pair(Integer.class, Long.class), value -> ((Integer) value).longValue());
        map.put(new Pair(Integer.class, Float.class), value -> ((Integer) value).floatValue());
        map.put(new Pair(Integer.class, Double.class), value -> ((Integer) value).doubleValue());
        map.put(new Pair(Long.class, Integer.class), value -> ((Long) value).intValue());
        map.put(new Pair(Long.class, Short.class), value -> ((Long) value).shortValue());
        map.put(new Pair(Long.class, Float.class), value -> ((Long) value).floatValue());
        map.put(new Pair(Long.class, Double.class), value -> ((Long) value).doubleValue());
        map.put(new Pair(Float.class, Short.class), value -> ((Float) value).shortValue());
        map.put(new Pair(Float.class, Integer.class), value -> ((Float) value).intValue());
        map.put(new Pair(Float.class, Long.class), value -> ((Float) value).longValue());
        map.put(new Pair(Float.class, Double.class), value -> ((Float) value).doubleValue());
        map.put(new Pair(Double.class, Short.class), value -> ((Double) value).shortValue());
        map.put(new Pair(Double.class, Integer.class), value -> ((Double) value).intValue());
        map.put(new Pair(Double.class, Long.class), value -> ((Double) value).longValue());
        map.put(new Pair(Double.class, Float.class), value -> ((Double) value).floatValue());
        map.put(new Pair(URI.class, URL.class), value -> {
            try {
                return ((URI) value).toURL();
            } catch (MalformedURLException e) {
                throw new DatatypeMappingException("Unable to transform URI to URL.", e);
            }
        });
        map.put(new Pair(BigInteger.class, Integer.class), value -> ((BigInteger) value).intValueExact());
        map.put(new Pair(BigInteger.class, Long.class), value -> ((BigInteger) value).longValueExact());
        return map;
    }

    /**
     * Converts the specified wrapper class to its corresponding primitive class.
     *
     * If the class parameter is a wrapper type, the equivalent primitive type will be returned (e.g. int.class for Integer.class)
     * In all other cases, the return value is null.
     *
     * @param cls - the class to convert
     * @return
     */
    public static Optional<Class<?>> wrapperTypeToPrimitiveType(Class<?> cls) {
        if(cls != null && WRAPPER_TO_PRIMITIVES.containsKey(cls)) {
            return Optional.of(WRAPPER_TO_PRIMITIVES.get(cls));
        }
        return Optional.empty();
    }

    /**
     * Maps the specified value to the target type (if possible).
     *
     * @param value      The value to convert
     * @param targetType The type to which the specified value should be converted
     * @param <T>        Target type
     * @return Value as the target type
     * @throws UnsupportedTypeTransformationException If the specified value cannot be transformed to the specified target type
     */
    public static <T> T transform(Object value, Class<T> targetType) {
        Objects.requireNonNull(targetType);
        if (value == null) {
            return null;
        }
        if (targetType.equals(String.class)) {
            return targetType.cast(value instanceof LangString ? ((LangString) value).getValue() : value.toString());
        }
        final Class<?> sourceType = value.getClass();
        if (targetType.isAssignableFrom(sourceType)) {
            return targetType.cast(value);
        }
        final Pair p = new Pair(sourceType, targetType);
        if (TRANSFORMERS.containsKey(p)) {
            return targetType.cast(TRANSFORMERS.get(p).apply(value));
        }
        final Optional<T> result = tryConversionUsingConstructor(value, targetType);
        return result.orElseThrow(() -> new UnsupportedTypeTransformationException(
                String.format("Cannot transform value %s of type %s to target type %s.", value, value.getClass(),
                        targetType)));
    }

    private static <T> Optional<T> tryConversionUsingConstructor(Object value, Class<T> targetType) {
        try {
            final Constructor<?>[] ctors = targetType.getDeclaredConstructors();
            for (Constructor<?> c : ctors) {
                if (c.getParameterCount() != 1) {
                    continue;
                }
                if (c.getParameterTypes()[0].isAssignableFrom(value.getClass())) {
                    return Optional.of(targetType.cast(c.newInstance(value)));
                }
            }
            return Optional.empty();
        } catch (IllegalAccessException | InstantiationException | InvocationTargetException e) {
            throw new DatatypeMappingException("Unable to transform value using target type constructor.", e);
        }
    }

}
