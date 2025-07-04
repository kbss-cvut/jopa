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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.ontodriver.model.InferenceMode;

import java.lang.reflect.Field;

public class PropertiesSpecificationImpl<X, Y, K, V> implements PropertiesSpecification<X, Y, K, V> {
    private final ManagedType<X> declaringType;
    private final FetchType fetchType;
    private final Field javaField;
    private final Class<Y> javaType;
    private final InferenceMode inferenceMode;
    private final Class<K> propertyIdType;
    private final Class<V> propertyValueType;

    private PropertiesSpecificationImpl(PropertiesSpecificationBuilder<X, Y, K, V> builder) {
        this.declaringType = builder.declaringType;
        this.fetchType = builder.fetchType;
        this.javaField = builder.javaField;
        this.javaType = builder.javaType;
        this.inferenceMode = builder.inferenceMode;
        this.propertyIdType = builder.propertyIdType;
        this.propertyValueType = builder.propertyValueType;
    }

    @Override
    public ManagedType<X> getDeclaringType() {
        return declaringType;
    }

    @Override
    public FetchType getFetchType() {
        return fetchType;
    }

    @Override
    public Field getJavaField() {
        return javaField;
    }

    @Override
    public Class<Y> getJavaType() {
        return javaType;
    }

    @Override
    public boolean isInferred() {
        return inferenceMode != InferenceMode.EXPLICIT;
    }

    @Override
    public boolean includeExplicit() {
        return inferenceMode != InferenceMode.INFERRED;
    }

    @Override
    public String getName() {
        return javaField.getName();
    }

    @Override
    public boolean isCollection() {
        return true;
    }

    @Override
    public Class<K> getPropertyIdentifierType() {
        return propertyIdType;
    }

    @Override
    public Class<V> getPropertyValueType() {
        return propertyValueType;
    }

    public static <X, Y, K, V> PropertiesSpecificationBuilder declaringType(ManagedType<X> declaringType) {
        return new PropertiesSpecificationBuilder<X, Y, K, V>().declaringType(declaringType);
    }

    public static class PropertiesSpecificationBuilder<X, Y, K, V> {
        private ManagedType<X> declaringType;
        private FetchType fetchType;
        private Field javaField;
        private Class<Y> javaType;
        private InferenceMode inferenceMode;
        private Class<K> propertyIdType;
        private Class<V> propertyValueType;

        public PropertiesSpecificationBuilder<X, Y, K, V> declaringType(ManagedType<X> declaringType) {
            this.declaringType = declaringType;
            return this;
        }

        public PropertiesSpecificationBuilder<X, Y, K, V> fetchType(FetchType fetchType) {
            this.fetchType = fetchType;
            return this;
        }

        public PropertiesSpecificationBuilder<X, Y, K, V> javaField(Field javaField) {
            this.javaField = javaField;
            return this;
        }

        public PropertiesSpecificationBuilder<X, Y, K, V> javaType(Class<Y> javaType) {
            this.javaType = javaType;
            return this;
        }

        public PropertiesSpecificationBuilder<X, Y, K, V> inferenceMode(InferenceMode inferenceMode) {
            this.inferenceMode = inferenceMode;
            return this;
        }

        public PropertiesSpecificationBuilder<X, Y, K, V> propertyIdType(Class<K> propertyIdType) {
            this.propertyIdType = propertyIdType;
            return this;
        }

        public PropertiesSpecificationBuilder<X, Y, K, V> propertyValueType(Class<V> propertyValueType) {
            this.propertyValueType = propertyValueType;
            return this;
        }

        public PropertiesSpecificationImpl<X, Y, K, V> build() {
            return new PropertiesSpecificationImpl<>(this);
        }
    }
}
