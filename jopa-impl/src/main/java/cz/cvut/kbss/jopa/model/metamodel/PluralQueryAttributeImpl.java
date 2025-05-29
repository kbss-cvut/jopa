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
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.utils.CollectionFactory;

import java.lang.reflect.Field;

/**
 * Plural query attributes can contain multiple values or references, i.e., they must be a collection.
 *
 * @param <X> The represented type that contains the attribute
 * @param <C> The type of the collection
 * @param <E> The type of the element inside the collection
 */
public class PluralQueryAttributeImpl<X, C, E> extends AbstractQueryAttribute<X, C>
        implements PluralQueryAttribute<X, C, E> {

    private final Type<E> elementType;

    private final Class<C> collectionType;

    public PluralQueryAttributeImpl(String query, boolean enableReferencingAttributes, Field field, ManagedType<X> declaringType, FetchType fetchType,
                                    ParticipationConstraint[] constraints, Type<E> elementType,
                                    Class<C> collectionType, ConverterWrapper converter) {
        super(query, enableReferencingAttributes, field, declaringType, fetchType, constraints, converter);
        this.elementType = elementType;
        this.collectionType = collectionType;
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
        return CollectionFactory.resolveCollectionType(getJavaType());
    }

    @Override
    public Type<E> getElementType() {
        return elementType;
    }

    @Override
    public Class<C> getJavaType() {
        return collectionType;
    }
}
