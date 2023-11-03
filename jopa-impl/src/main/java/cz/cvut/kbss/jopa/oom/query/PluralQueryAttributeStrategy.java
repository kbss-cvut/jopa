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
package cz.cvut.kbss.jopa.oom.query;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PluralQueryAttributeImpl;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.utils.CollectionFactory;

import java.util.Collection;

public class PluralQueryAttributeStrategy<X> extends QueryFieldStrategy<PluralQueryAttributeImpl<? super X, ?, ?>, X> {

    private final Collection<Object> values;

    public PluralQueryAttributeStrategy(EntityType<X> et, PluralQueryAttributeImpl<? super X, ?, ?> attribute) {
        super(et, attribute);
        this.values = CollectionFactory.createDefaultCollection(attribute.getCollectionType());
    }

    @Override
    public void addValueFromTypedQuery(TypedQuery<?> typedQuery) {
        typedQuery.getResultStream().map(this::toAttributeValue).forEach(values::add);
    }

    @Override
    public void buildInstanceFieldValue(Object instance) {
        if (!values.isEmpty()) {
            setValueOnInstance(instance, values);
        }
    }
}
