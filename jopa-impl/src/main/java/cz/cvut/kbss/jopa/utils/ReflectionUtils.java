/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.exception.InstantiationException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Utility functions using Java Reflection API.
 */
public class ReflectionUtils {

    private ReflectionUtils() {
        throw new AssertionError();
    }

    /**
     * Creates a new instance of the specified class using the default no-arg constructor.
     * <p>
     * Note that it is expected that the no-arg constructor exists and is publicly accessible.
     *
     * @param cls Class to instantiate
     * @param <T> Type
     * @return New instance of class {@code entityClass}
     * @throws InstantiationException When no-arg constructor does not exist or is not accessible
     */
    public static <T> T instantiateUsingDefaultConstructor(Class<T> cls) {
        // The main purpose of this method is to wrap object instantiation so that the deprecated Class.newInstance
        // calls can be replaced after migrating to newer Java version
        try {
            return cls.getDeclaredConstructor().newInstance();
        } catch (java.lang.InstantiationException | IllegalAccessException | NoSuchMethodException |
                 InvocationTargetException e) {
            throw new InstantiationException(e);
        }
    }

    /**
     * Checks if the specified class overrides the {@link Object#equals(Object)} method.
     *
     * @param cls Class to investigate
     * @return {@code true} if the class overrides {@code equals}, {@code false} otherwise
     */
    public static boolean overridesEquals(Class<?> cls) {
        try {
            final Method m = cls.getDeclaredMethod("equals", Object.class);
            return !m.getDeclaringClass().equals(Object.class);
        } catch (NoSuchMethodException e) {
            return false;
        }
    }

    /**
     * Checks if the specified class overrides the {@link Object#hashCode()} method.
     *
     * @param cls Class to investigate
     * @return {@code true} if the class overrides {@code hasCode}, {@code false} otherwise
     */
    public static boolean overridesHashCode(Class<?> cls) {
        try {
            final Method m = cls.getDeclaredMethod("hashCode");
            return !m.getDeclaringClass().equals(Object.class);
        } catch (NoSuchMethodException e) {
            return false;
        }
    }
}
