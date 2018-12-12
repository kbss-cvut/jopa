/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;

import java.lang.reflect.Field;
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
    public boolean isAssociation() {
        return getPersistentAttributeType().equals(
                PersistentAttributeType.OBJECT);
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
    public cz.cvut.kbss.jopa.model.metamodel.PluralAttribute.CollectionType getCollectionType() {
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

    public abstract static class PluralAttributeBuilder<X, C, E> extends AbstractAttributeBuilder<X, C> {
        private Type<E> elementType;
        private Class<C> collectionType;

        public PluralAttributeBuilder<X, C, E> elementType(Type<E> elementType) {
            this.elementType = elementType;
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> constraints(ParticipationConstraint[] constraints) {
            super.constraints(constraints);
            return this;
        }

        public PluralAttributeBuilder<X, C, E> collectionType(Class<C> collectionType) {
            this.collectionType = collectionType;
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> field(Field field) {
            super.field(field);
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> declaringType(ManagedType<X> declaringType) {
            super.declaringType(declaringType);
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> attributeType(PersistentAttributeType attributeType) {
            super.attributeType(attributeType);
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> iri(IRI iri) {
            super.iri(iri);
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> cascadeTypes(CascadeType[] cascadeTypes) {
            super.cascadeTypes(cascadeTypes);
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> fetchType(FetchType fetchType) {
            super.fetchType(fetchType);
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> inferred(boolean inferred) {
            super.inferred(inferred);
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> includeExplicit(boolean includeExplicit) {
            super.includeExplicit(includeExplicit);
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> nonEmpty(boolean nonEmpty) {
            super.nonEmpty(nonEmpty);
            return this;
        }

        public PluralAttributeBuilder<X, C, E> participationConstraints(ParticipationConstraint[] constraints) {
            super.constraints(constraints);
            return this;
        }

        @Override
        public PluralAttributeBuilder<X, C, E> converter(ConverterWrapper converter) {
            super.converter(converter);
            return this;
        }

        public abstract PluralAttribute<X, C, E> build();
    }
}
