/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.proxy.lazy;

import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.CollectionFactory;

import java.util.Set;

public class LazyLoadingSetProxy<O, E> extends LazyLoadingCollectionProxy<O, Set<E>, E> implements Set<E> {
    public LazyLoadingSetProxy(O owner, FieldSpecification<? super O, Set<E>> fieldSpec,
                               UnitOfWork persistenceContext) {
        super(owner, fieldSpec, persistenceContext);
    }

    @Override
    public Set<E> unwrap() {
        return (Set<E>) CollectionFactory.createDefaultCollection(CollectionType.SET);
    }
}
