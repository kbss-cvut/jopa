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

import java.util.Collection;
import java.util.Set;

public class CollectionAttributeImpl<X, V> extends AbstractPluralAttribute<X, Collection<V>, V>
        implements CollectionAttribute<X, V> {

    private CollectionAttributeImpl(PluralAttributeBuilder<X, Collection<V>, V> builder) {
        super(builder);
    }

    @Override
    public CollectionType getCollectionType() {
        return CollectionType.COLLECTION;
    }

    @Override
    public String toString() {
        return "CollectionAttribute[" + getName() + "]";
    }

    static PluralAttributeBuilder builder(PropertyAttributes config) {
        return new CollectionAttributeBuilder().collectionType(Set.class).config(config);
    }

    static class CollectionAttributeBuilder<X, V> extends PluralAttributeBuilder<X, Collection<V>, V> {

        @Override
        public CollectionAttributeImpl<X, V> build() {
            final CollectionAttributeImpl<X, V> result = new CollectionAttributeImpl<>(this);
            mappingValidator.validateAttributeMapping(result);
            return result;
        }
    }
}
