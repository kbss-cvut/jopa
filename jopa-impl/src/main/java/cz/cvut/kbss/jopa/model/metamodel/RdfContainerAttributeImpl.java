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

import cz.cvut.kbss.jopa.exception.InvalidFieldMappingException;
import cz.cvut.kbss.jopa.model.annotations.RDFContainerType;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.ontodriver.model.InferenceMode;

public class RdfContainerAttributeImpl<X, C, E> extends AbstractPluralAttribute<X, C, E> implements RDFContainerAttribute<X, C, E> {

    private final RDFContainerType containerType;

    private final CollectionType collectionType;

    RdfContainerAttributeImpl(RDFContainerAttributeBuilder<X, C, E> builder) {
        super(builder);
        this.containerType = builder.containerType;
        this.collectionType = resolveCollectionType(containerType);
        validateCollectionType();
    }

    private static CollectionType resolveCollectionType(RDFContainerType containerType) {
        return switch (containerType) {
            case SEQ -> CollectionType.LIST;
            case ALT -> CollectionType.SET;
            case BAG -> CollectionType.COLLECTION;
        };
    }

    private void validateCollectionType() {
        if (containerType == RDFContainerType.SEQ && CollectionType.SET.getCollectionClass()
                                                                       .isAssignableFrom(getJavaType())
                || containerType == RDFContainerType.ALT && CollectionType.LIST.getCollectionClass()
                                                                               .isAssignableFrom(getJavaType())) {
            throw new InvalidFieldMappingException("RDF " + containerType.name() + " cannot be mapped to a field of type " + getJavaType());
        }
    }

    @Override
    public RDFContainerType getContainerType() {
        return containerType;
    }

    @Override
    public CollectionType getCollectionType() {
        return collectionType;
    }

    @Override
    public String toString() {
        return "RDFContainerAttribute[" + getName() + "]";
    }

    static RDFContainerAttributeBuilder builder(PropertyAttributes config) {
        return new RDFContainerAttributeBuilder().config(config);
    }

    static class RDFContainerAttributeBuilder<X, C, E> extends PluralAttributeBuilder<X, C, E> {

        private RDFContainerType containerType;

        public RDFContainerAttributeBuilder<X, C, E> containerType(RDFContainerType containerType) {
            this.containerType = containerType;
            return this;
        }

        @Override
        public RDFContainerAttributeBuilder<X, C, E> elementType(Type<E> elementType) {
            super.elementType(elementType);
            return this;
        }

        @Override
        public RDFContainerAttributeBuilder<X, C, E> collectionType(Class<C> collectionType) {
            super.collectionType(collectionType);
            return this;
        }

        @Override
        public RDFContainerAttributeBuilder<X, C, E> propertyInfo(PropertyInfo propertyInfo) {
            super.propertyInfo(propertyInfo);
            return this;
        }

        @Override
        public RDFContainerAttributeBuilder<X, C, E> declaringType(ManagedType<X> declaringType) {
            super.declaringType(declaringType);
            return this;
        }

        @Override
        public RDFContainerAttributeBuilder<X, C, E> inferenceMode(InferenceMode inferenceMode) {
            super.inferenceMode(inferenceMode);
            return this;
        }

        @Override
        public RDFContainerAttributeBuilder<X, C, E> converter(ConverterWrapper converter) {
            super.converter(converter);
            return this;
        }

        @Override
        public RDFContainerAttributeBuilder<X, C, E> config(PropertyAttributes config) {
            super.config(config);
            return this;
        }

        @Override
        public RdfContainerAttributeImpl<X, C, E> build() {
            return new RdfContainerAttributeImpl<>(this);
        }
    }
}
