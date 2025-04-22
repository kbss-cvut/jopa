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
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.ontodriver.model.InferenceMode;

import java.util.List;

public class RDFCollectionAttribute<X, V> extends ListAttributeImpl<X, V> {

    RDFCollectionAttribute(RDFCollectionAttributeBuilder<X, V> builder) {
        super(builder);
    }

    @Override
    public boolean isRDFCollection() {
        return true;
    }

    @Override
    public String toString() {
        return "RDFCollectionAttribute[" + getName() + "]";
    }

    static RDFCollectionAttributeBuilder builder(PropertyAttributes config) {
        return new RDFCollectionAttributeBuilder().collectionType(List.class).config(config);
    }

    static class RDFCollectionAttributeBuilder<X, V> extends ListAttributeBuilder<X, V> {

        RDFCollectionAttributeBuilder() {
            owlListClass(IRI.create(RDF.LIST));
            hasNextProperty(IRI.create(RDF.REST));
            hasContentsProperty(IRI.create(RDF.FIRST));
            sequenceType(SequenceType.referenced);
        }

        @Override
        public RDFCollectionAttributeBuilder<X, V> config(PropertyAttributes config) {
            super.config(config);
            return this;
        }

        @Override
        public RDFCollectionAttributeBuilder<X, V> collectionType(Class<List<V>> collectionType) {
            super.collectionType(collectionType);
            return this;
        }

        @Override
        public RDFCollectionAttributeBuilder<X, V> elementType(Type<V> elementType) {
            super.elementType(elementType);
            return this;
        }

        @Override
        public RDFCollectionAttributeBuilder<X, V> propertyInfo(PropertyInfo propertyInfo) {
            super.propertyInfo(propertyInfo);
            return this;
        }

        @Override
        public RDFCollectionAttributeBuilder<X, V> declaringType(ManagedType<X> declaringType) {
            super.declaringType(declaringType);
            return this;
        }

        @Override
        public RDFCollectionAttributeBuilder<X, V> inferenceMode(InferenceMode inferenceMode) {
            super.inferenceMode(inferenceMode);
            return this;
        }

        @Override
        public RDFCollectionAttributeBuilder<X, V> converter(ConverterWrapper converter) {
            super.converter(converter);
            return this;
        }

        @Override
        public RDFCollectionAttribute<X, V> build() {
            return new RDFCollectionAttribute<>(this);
        }
    }
}
