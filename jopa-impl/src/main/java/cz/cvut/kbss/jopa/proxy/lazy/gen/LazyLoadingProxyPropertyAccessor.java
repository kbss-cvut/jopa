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
package cz.cvut.kbss.jopa.proxy.lazy.gen;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

/**
 * Provides access to persistence context-related attributes needed by lazy loading proxies.
 * <p>
 * This interface should be implemented by the classes generated by {@link LazyLoadingEntityProxyGenerator}.
 */
public interface LazyLoadingProxyPropertyAccessor<T> {

    UnitOfWork getPersistenceContext();

    void setPersistenceContext(UnitOfWork uow);

    FieldSpecification<?, ?> getFieldSpec();

    void setFieldSpec(FieldSpecification<?, ?> fieldSpec);

    Object getOwner();

    void setOwner(Object owner);

    void setValue(T value);
}
