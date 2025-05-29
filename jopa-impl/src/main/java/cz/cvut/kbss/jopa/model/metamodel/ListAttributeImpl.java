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

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;

import java.util.List;

public class ListAttributeImpl<X, V> extends AbstractPluralAttribute<X, List<V>, V>
        implements ListAttribute<X, V> {

    private final IRI listClass;

    private final IRI hasNextProperty;

    private final IRI hasContentsProperty;

    private final SequenceType owlSequenceType;

    ListAttributeImpl(ListAttributeBuilder<X, V> builder) {
        super(builder);
        this.listClass = builder.listClass;
        this.hasNextProperty = builder.hasNextProperty;
        this.hasContentsProperty = builder.hasContentsProperty;
        this.owlSequenceType = builder.owlSequenceType;
    }

    @Override
    public CollectionType getCollectionType() {
        return CollectionType.LIST;
    }

    @Override
    public IRI getListClassIRI() {
        return listClass;
    }

    @Override
    public IRI getHasNextPropertyIRI() {
        return hasNextProperty;
    }

    @Override
    public IRI getHasContentsPropertyIRI() {
        return hasContentsProperty;
    }

    @Override
    public SequenceType getSequenceType() {
        return owlSequenceType;
    }

    @Override
    public String toString() {
        return "ListAttribute[" + getName() + "]";
    }

    static ListAttributeBuilder builder(PropertyAttributes config) {
        return new ListAttributeBuilder().collectionType(List.class).config(config);
    }

    static class ListAttributeBuilder<X, V> extends PluralAttributeBuilder<X, List<V>, V> {
        private IRI listClass;
        private IRI hasNextProperty;
        private IRI hasContentsProperty;
        private SequenceType owlSequenceType;

        @Override
        public ListAttributeBuilder<X, V> config(PropertyAttributes config) {
            super.config(config);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> collectionType(Class<List<V>> collectionType) {
            super.collectionType(collectionType);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> propertyInfo(PropertyInfo propertyInfo) {
            super.propertyInfo(propertyInfo);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> declaringType(ManagedType<X> declaringType) {
            super.declaringType(declaringType);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> converter(ConverterWrapper converter) {
            super.converter(converter);
            return this;
        }

        public ListAttributeBuilder<X, V> owlListClass(IRI owlListClass) {
            this.listClass = owlListClass;
            return this;
        }

        public ListAttributeBuilder<X, V> hasNextProperty(IRI hasNextProperty) {
            this.hasNextProperty = hasNextProperty;
            return this;
        }

        public ListAttributeBuilder<X, V> hasContentsProperty(IRI hasContentsProperty) {
            this.hasContentsProperty = hasContentsProperty;
            return this;
        }

        public ListAttributeBuilder<X, V> sequenceType(SequenceType sequenceType) {
            this.owlSequenceType = sequenceType;
            return this;
        }

        @Override
        public ListAttributeImpl<X, V> build() {
            final ListAttributeImpl<X, V> result = new ListAttributeImpl<>(this);
            mappingValidator.validateAttributeMapping(result);
            return result;
        }
    }
}
