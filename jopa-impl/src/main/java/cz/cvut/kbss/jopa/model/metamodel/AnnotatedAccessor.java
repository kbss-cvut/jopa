/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;

import java.beans.Introspector;
import java.lang.reflect.Method;

/**
 * Class used for extracting field name from getter or setter.
 */
public abstract class AnnotatedAccessor {
    public static final String GET_PREFIX = "get";
    public static final String SET_PREFIX = "set";
    public static final String IS_PREFIX = "is";
    public static final String HAS_PREFIX = "has";

    protected final Method method;
    protected final String propertyName;
    protected final Class<?> propertyType;

    protected AnnotatedAccessor(Method method) {
        this.method = method;
        this.propertyName = extractPropertyNameFromMethod(method);
        this.propertyType = extractPropertyTypeNameFromMethod(method);
    }

    public static AnnotatedAccessor from(Method method) {
        if (isSetter(method)) {
            return new AnnotatedSetter(method);
        } else if (isGetter(method)) {
            return new AnnotatedGetter(method);
        } else {
            throw new MetamodelInitializationException("Method " + method + " is neither a setter nor a getter. Only simple getters and setters following the Java naming convection can be used for OWL property annotations.");
        }

    }

    protected abstract String extractPropertyNameFromMethod(Method m);

    protected abstract Class<?> extractPropertyTypeNameFromMethod(Method m);

    private static boolean isSetter(Method m) {
        return m.getName().startsWith(SET_PREFIX) && m.getParameterCount() == 1;
    }

    private static boolean isGetter(Method m) {
        if (m.getParameterCount() != 0 || m.getReturnType()
                                           .equals(Void.TYPE)) { /// getter has 0 arguments and returns something
            return false;
        }

        if (m.getName().startsWith(GET_PREFIX)) {
            return true;
        } else if (m.getReturnType().isAssignableFrom(Boolean.class) || m.getReturnType()
                                                                         .isAssignableFrom(boolean.class)) {/// getter on bools - can start with is,get or has
            return m.getName().startsWith(IS_PREFIX) || m.getName().startsWith(HAS_PREFIX);
        } else {
            return false;
        }
    }

    public Method getMethod() {
        return method;
    }

    public String getPropertyName() {
        return propertyName;
    }

    public Class<?> getPropertyType() {
        return propertyType;
    }

    private static class AnnotatedSetter extends AnnotatedAccessor {

        public AnnotatedSetter(Method method) {
            super(method);
        }

        @Override
        protected Class<?> extractPropertyTypeNameFromMethod(Method m) {
            return m.getParameterTypes()[0];
        }

        @Override
        protected String extractPropertyNameFromMethod(Method m) {
            String trimmedName = m.getName().substring(SET_PREFIX.length()); /// always starts with set
            return Introspector.decapitalize(trimmedName);
        }
    }

    private static class AnnotatedGetter extends AnnotatedAccessor {
        public AnnotatedGetter(Method method) {
            super(method);
        }

        @Override
        protected String extractPropertyNameFromMethod(Method m) {
            String trimmedName;

            if (m.getName().startsWith(IS_PREFIX)) {
                trimmedName = m.getName().substring(IS_PREFIX.length());
            } else if (m.getName().startsWith(GET_PREFIX) || m.getName().startsWith(HAS_PREFIX)) {
                trimmedName = m.getName().substring(GET_PREFIX.length());
            } else {
                throw new MetamodelInitializationException("Failed to extract property name from annotated getter.");
            }
            return Introspector.decapitalize(trimmedName);
        }

        @Override
        protected Class<?> extractPropertyTypeNameFromMethod(Method m) {
            return m.getReturnType();
        }
    }

    @Override
    public String toString() {
        return "AnnotatedAccessor{" +
                "method=" + method +
                ", propertyType=" + propertyType +
                '}';
    }
}
