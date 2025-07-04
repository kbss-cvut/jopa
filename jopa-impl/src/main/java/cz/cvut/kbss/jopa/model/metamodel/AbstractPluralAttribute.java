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

import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.ontodriver.model.InferenceMode;

import java.util.Collection;
import java.util.Map;

public abstract class AbstractPluralAttribute<X, C, E> extends AbstractAttribute<X, C>
        implements PluralAttribute<X, C, E> {

    private final Type<E> elementType;

    private final Class<C> collectionType;

    AbstractPluralAttribute(PluralAttributeBuilder<X, C, E> builder) {
        super(builder);
        this.elementType = builder.elementType;
        this.collectionType = builder.collectionType;
    }

    @Override
    public boolean isCollection() {
        return true;
    }

    @Override
    public Class<E> getBindableJavaType() {
        return elementType.getJavaType();
    }

    @Override
    public cz.cvut.kbss.jopa.model.metamodel.Bindable.BindableType getBindableType() {
        return BindableType.PLURAL_ATTRIBUTE;
    }

    @Override
    public cz.cvut.kbss.jopa.model.metamodel.CollectionType getCollectionType() {
        if (getJavaType().isAssignableFrom(Collection.class)) {
            return CollectionType.COLLECTION;
        } else if (getJavaType().isAssignableFrom(Map.class)) {
            return CollectionType.MAP;
        } else {
            throw new IllegalArgumentException();
        }
    }

    @Override
    public Type<E> getElementType() {
        return elementType;
    }

    @Override
    public Class<C> getJavaType() {
        return collectionType;
    }

    abstract static class PluralAttributeBuilder<X, C, E> extends AbstractAttributeBuilder<X, C> {
        private Type<E> elementType;
        private Class<C> collectionType;

        @Override
        public PluralAttributeBuilder<X, C, E> config(PropertyAttributes config) {
            super.config(config);
            elementType((Type<E>) config.getType());
            return this;
        }

        public PluralAttributeBuilder<X, C, E> elementType(Type<E> elementType) {
            this.elementType = elementType;
            return this;
        }

        public PluralAttributeBuilder<X, C, E> collectionType(Class<C> collectionType) {
            this.collectionType = collectionType;
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> propertyInfo(PropertyInfo propertyInfo) {
            super.propertyInfo(propertyInfo);
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> declaringType(ManagedType<X> declaringType) {
            super.declaringType(declaringType);
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> inferenceMode(InferenceMode inferenceMode) {
            super.inferenceMode(inferenceMode);
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> converter(ConverterWrapper converter) {
            super.converter(converter);
            return this;
        }

        @Override
        public abstract AbstractPluralAttribute<X, C, E> build();
    }
}
